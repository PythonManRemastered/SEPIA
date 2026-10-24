import contextlib
import datetime as dt
import io
import math
import numbers
import os
import time
import uuid

VERSION = "0.3.3"
ENDPOINT = "https://mpmprodvdmml.ondemand.sas.com/cas-shared-default-http/"


class RemoteError(Exception):
    def __init__(self, code, message):
        self.code, self.message = code, message


def scalar(value):

    import pandas as pd
    if value is None or bool(pd.isna(value)):
        return {"SEPIAType": "Missing", "Reason": "CASMissing"}
    if isinstance(value, (str, bool)):
        return value
    if isinstance(value, (dt.datetime, dt.date, dt.time)):
        kind = "DateTime" if isinstance(value, dt.datetime) else type(value).__name__.title()
        return {"SEPIAType": kind, "ISO8601": value.isoformat()}
    if isinstance(value, numbers.Integral):
        return int(value)
    if isinstance(value, numbers.Real):
        value = float(value)
        if not math.isfinite(value):
            raise RemoteError("NonFiniteValue", "An infinite numeric value cannot be transferred.")
        return value
    raise RemoteError("UnsupportedValue", "Unsupported cell type: " + type(value).__name__)


def relative_path(value, allow_empty=False):
    if not isinstance(value, str) or (not value and not allow_empty):
        raise RemoteError("InvalidPath", "Specify a relative path returned by the file listing.")
    if value.startswith("/") or "\\" in value or any(p == ".." for p in value.split("/")) or "\x00" in value:
        raise RemoteError("InvalidPath", "Absolute paths and parent traversal are not accepted.")
    return value


class Bridge:
    def __init__(self):
        self.connection = None
        self.tables = {}
        self.listings = {}
        self.swat_version = None

    def dispatch(self, operation, arguments):
        started = time.monotonic()
        allowed = {"connect", "files", "load", "fetch", "drop", "close", "browse", "select", "inspect"}
        try:
            if operation not in allowed:
                raise RemoteError("UnknownOperation", "Unknown SEPIA operation.")
            if operation != "connect" and operation != "close" and self.connection is None:
                raise RemoteError("NotConnected", "Open a PDS connection first.")

            with contextlib.redirect_stdout(io.StringIO()), contextlib.redirect_stderr(io.StringIO()):
                result = getattr(self, operation)(*arguments)
            return {"OK": True, **result, "ElapsedSeconds": round(time.monotonic() - started, 3)}
        except RemoteError as error:
            return {"OK": False, "Code": error.code, "Message": error.message,
                    "ElapsedSeconds": round(time.monotonic() - started, 3)}
        except Exception as error:

            text = str(error).lower()
            if "401" in text or "unauthorized" in text:
                message = "Authentication rejected. Use the SAS username shown by PDS, not the website email."
            elif "certificate" in text or "ssl" in text:
                message = "TLS verification failed. Do not disable verification; inspect the certificate configuration."
            else:
                message = "The remote operation failed. Check the session and source metadata; no automatic retry was made."
            return {"OK": False, "Code": type(error).__name__, "Message": message,
                    "ElapsedSeconds": round(time.monotonic() - started, 3)}

    @staticmethod
    def checked(result, operation):
        severity = int(result.severity)
        if severity >= 2:
            raise RemoteError("CASActionFailed", operation + " failed on SAS (severity " + str(severity) +
                              "). Check the exact caslib/path and access. No fallback source was used.")
        return severity

    def connect(self, username, password):
        import certifi
        import swat
        if self.connection is not None:
            raise RemoteError("AlreadyConnected", "Close the existing connection before reconnecting.")
        if not isinstance(username, str) or not username.strip() or not isinstance(password, str) or not password:
            raise RemoteError("EmptyCredentials", "Both SAS username and password are required.")
        bundle = certifi.where()
        if not os.path.isfile(bundle):
            raise RemoteError("MissingCertificateBundle", "The certifi bundle was not found.")
        os.environ["CAS_CLIENT_SSL_CA_LIST"] = bundle
        os.environ["REQUESTS_CA_BUNDLE"] = bundle
        try:
            self.connection = swat.CAS(ENDPOINT, username=username.strip(), password=password)
            self.swat_version = swat.__version__
            return {"Status": "Connected", "Endpoint": ENDPOINT, "SWATVersion": self.swat_version,
                    "ConnectorVersion": VERSION}
        finally:
            password = None

    def files(self, caslib, path=""):
        if not isinstance(caslib, str) or not caslib.strip():
            raise RemoteError("InvalidCaslib", "An exact caslib identifier is required.")
        relative_path(path, allow_empty=True)
        kwargs = {"caslib": caslib, "allFiles": True}
        if path:
            kwargs["path"] = path
        result = self.connection.table.fileInfo(**kwargs)
        severity = self.checked(result, "File listing")
        if "FileInfo" not in result:
            raise RemoteError("UnexpectedResponse", "SAS returned no FileInfo table.")
        frame = result["FileInfo"]
        entries = []
        for _, row in frame.iterrows():
            entries.append({"Name": str(row["Name"]), "Bytes": int(row["Size"]),
                            "Permissions": str(row.get("Permission", ""))})
        return {"Caslib": caslib, "Directory": path, "Entries": entries, "Severity": severity}

    def browse(self, uid, path="", caslib=None):
        if not isinstance(uid, str) or not uid.strip():
            raise RemoteError("InvalidUID", "Enter a nonempty contribution identifier.")
        if caslib is None:
            caslib = uid
        relative_path(path, allow_empty=True)
        path = path.rstrip("/")
        listing = self.files(caslib, path)
        entries = []
        for raw in listing["Entries"]:
            name = raw["Name"]
            if name in (".", ".."):
                continue
            relative_path(name)

            if "/" not in name.rstrip("/"):
                full = (path + "/" if path else "") + name.rstrip("/")
            elif not path or name.startswith(path + "/"):
                full = name.rstrip("/")
            else:
                raise RemoteError("AmbiguousListingPath", "Server returned an ambiguous entry path; no path was guessed.")
            kind = "Directory" if raw["Permissions"].startswith("d") else (
                "File" if raw["Permissions"].startswith("-") else "Unknown")
            supported = kind == "File" and full.lower().endswith(".sas7bdat")
            entries.append({**raw, "Index": len(entries) + 1, "RelativePath": full,
                            "Kind": kind, "SupportedImport": supported})
        token = uuid.uuid4().hex
        answer = {"ListingToken": token, "ContributionID": uid, "Caslib": caslib,
                  "IdentityStatus": "UserSuppliedNotVerified", "Directory": path,
                  "ParentDirectory": path.rpartition("/")[0], "Entries": entries,
                  "Severity": listing["Severity"]}
        self.listings[token] = answer
        return answer

    def select(self, listing_token, index):
        if listing_token not in self.listings:
            raise RemoteError("UnknownListing", "Browse again in the current session.")
        listing = self.listings[listing_token]
        if isinstance(index, bool) or not isinstance(index, int) or not 1 <= index <= len(listing["Entries"]):
            raise RemoteError("InvalidSelection", "Choose an Index from the displayed listing.")
        entry = listing["Entries"][index - 1]
        if entry["Kind"] == "Directory":
            return self.browse(listing["ContributionID"], entry["RelativePath"], listing["Caslib"])
        if not entry["SupportedImport"]:
            raise RemoteError("UnsupportedEntry", "Only regular SAS7BDAT files can be loaded; folders can be opened. Archives and documents are not imported.")
        loaded = self.load(listing["Caslib"], entry["RelativePath"])
        identity = {"ContributionID": listing["ContributionID"], "IdentityStatus": "UserSuppliedNotVerified"}
        self.tables[loaded["Token"]].update(identity)
        return {**loaded, **identity}

    def inspect(self, token):
        record = self.record(token)
        table = self.connection.CASTable(record["TemporaryTable"], caslib="CASUSER")
        rows, columns = map(int, table.shape)
        names = list(table.columns)
        if len(names) != columns or len(set(names)) != columns or not all(isinstance(n, str) for n in names):
            raise RemoteError("ColumnMismatch", "Source column metadata is inconsistent.")
        return {"Dimensions": [rows, columns], "ColumnNames": names,
                "LoadSeverity": record["LoadSeverity"],
                "LoadStatus": "Warning" if record["LoadSeverity"] == 1 else "Success",
                "MetadataStatus": "Retrieved", "Provenance": dict(record)}

    def load(self, caslib, path):
        relative_path(path)
        if not isinstance(caslib, str) or not caslib.strip():
            raise RemoteError("InvalidCaslib", "An exact caslib identifier is required.")
        if not path.lower().endswith(".sas7bdat"):
            raise RemoteError("UnsupportedFormat", "This prototype loads SAS7BDAT files only.")
        token = uuid.uuid4().hex
        name = "sepia_" + token[:20]
        record = {"Token": token, "Caslib": caslib, "RelativePath": path,
                  "TemporaryTable": name, "LoadedAtUTC": dt.datetime.now(dt.timezone.utc).isoformat()}
        loaded = self.connection.table.loadTable(
            sourceCaslib=caslib, path=path,
            casout={"name": name, "caslib": "CASUSER", "replace": False, "promote": False})
        severity = self.checked(loaded, "Table load")

        self.tables[token] = record
        table = self.connection.CASTable(name, caslib="CASUSER")
        rows, columns = table.shape
        record.update(Rows=int(rows), Columns=int(columns), LoadSeverity=severity)
        return dict(record)

    def record(self, token):
        if token not in self.tables:
            raise RemoteError("UnknownTable", "This table token is not owned by the current connection.")
        return self.tables[token]

    def fetch(self, token, max_rows=1000, max_cells=200000):
        record = self.record(token)
        for limit in (max_rows, max_cells):
            if isinstance(limit, bool) or not isinstance(limit, int) or limit < 1:
                raise RemoteError("InvalidLimit", "Fetch limits must be positive integers.")
        table = self.connection.CASTable(record["TemporaryTable"], caslib="CASUSER")
        rows, columns = map(int, table.shape)
        if rows > max_rows or rows * columns > max_cells:
            raise RemoteError("FetchLimit", "Table exceeds the requested row or cell limit; no rows were fetched.")

        fetched = table.fetch(to=max(1, rows), maxRows=max(1, rows))
        severity = self.checked(fetched, "Table fetch")
        if "Fetch" not in fetched:
            raise RemoteError("UnexpectedResponse", "SAS returned no Fetch table.")
        frame = fetched["Fetch"]

        names = list(table.columns)
        if len(names) != columns or len(set(names)) != columns or not all(isinstance(n, str) for n in names):
            raise RemoteError("ColumnMismatch", "Source column metadata is inconsistent.")
        if len(frame) != rows or any(n not in frame.columns for n in names):
            raise RemoteError("IncompleteFetch", "Fetched rows/columns do not match the complete table; no partial data returned.")
        frame = frame.loc[:, names]
        data = [[scalar(value) for value in row] for row in frame.itertuples(index=False, name=None)]
        provenance = {**record, "Endpoint": ENDPOINT, "AccessMethod": "SAS_CAS_REST",
                      "FetchedAtUTC": dt.datetime.now(dt.timezone.utc).isoformat(),
                      "ConnectorVersion": VERSION, "SWATVersion": self.swat_version,
                      "OriginalFileSHA256": "NotComputed", "RowOrder": "Unspecified"}
        return {"ColumnNames": names, "Data": data,
                "TransportDTypes": {n: str(frame[n].dtype) for n in names},
                "Dimensions": [rows, columns], "Severity": severity, "Provenance": provenance,
                "Warnings": ["CAS data are not a lossless representation of the original SAS file.",
                             "Missing SAS tags may be collapsed by CAS/SWAT. Empty strings are preserved.",
                             "Temporal values use explicit ISO8601 tagged associations; no clinical recoding.",
                             "Align comparisons by verified participant/record keys, not row positions."]}

    def drop(self, token):
        record = self.record(token)
        result = self.connection.table.dropTable(name=record["TemporaryTable"], caslib="CASUSER")
        severity = self.checked(result, "Temporary table cleanup")
        del self.tables[token]
        return {"Status": "Dropped", "Severity": severity}

    def close(self):
        if self.connection is None:
            return {"Status": "AlreadyClosed"}
        self.connection.close()
        self.connection = None
        self.tables.clear()
        self.listings.clear()
        return {"Status": "Closed"}


bridge = Bridge()
