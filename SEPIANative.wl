BeginPackage["SEPIANative`"];
PDSNativeConnect;
PDSNativeListFiles;
PDSNativeLoadTable;
PDSNativeInspectTable;
PDSNativeFetchTable;
PDSNativeCompareTables;
PDSNativeBrowseContribution;
PDSNativeOpenEntry;
PDSNativeOpenBrowser;
PDSNativeImportedData;
PDSNativeClearImportedData;
PDSNativeOpenZIP;
PDSNativeLoadZIPMember;
PDSNativeCloseZIP;
PDSNativeUploadSAS;
PDSNativeDropTable;
PDSNativeDisconnect;

Begin["`Private`"];
$endpoint = "https://mpmprodvdmml.ondemand.sas.com/cas-shared-default-http/";
$version = "0.6.0";
If[!ValueQ[$connection], $connection = None];
If[!ValueQ[$busy], $busy = False];
If[!ValueQ[$tables], $tables = <||>];
If[!ValueQ[$listings], $listings = <||>];

failure[code_, message_, details_: <||>] :=
  Failure[code, Join[<|"MessageTemplate" -> message|>, details]];

validTimeout[t_] := NumberQ[t] && TrueQ[0 < t <= 300];
validSession[s_] := StringQ[s] && StringMatchQ[s,
  RegularExpression["[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}"]];
validPath[p_] := StringQ[p] && !StringStartsQ[p, "/"] &&
  !StringContainsQ[p, {"\\", FromCharacterCode[0]}] && !MemberQ[StringSplit[p, "/"], ".."];

SetAttributes[run, HoldAll];
run[body_] := Module[{started, result},
  If[TrueQ[$busy], Return[failure["Busy", "Wait for the current native SAS operation to finish."]]];
  started = AbsoluteTime[];
  result = WithCleanup[$busy = True, body, $busy = False];
  If[AssociationQ[result], Append[result, "ElapsedSeconds" -> Round[AbsoluteTime[] - started, .001]],
    If[FailureQ[result], Failure[result[[1]], Append[result[[2]],
      "ElapsedSeconds" -> Round[AbsoluteTime[] - started, .001]]], result]]
];

transport[req_, timeout_] := Quiet[Check[
  URLRead[req, {"StatusCode", "BodyByteArray", "Cookies"},
    TimeConstraint -> timeout, FollowRedirects -> False,
    VerifySecurityCertificates -> True, Interactive -> False, CookieFunction -> None], $Failed]];

parseJSON[bytes_ByteArray] := Quiet[Check[ImportByteArray[bytes, "RawJSON"], $Failed]];
parseJSON[text_String] := parseJSON[StringToByteArray[text, "UTF-8"]];
parseJSON[_] := $Failed;

request[method_, route_, body_, auth_, affinity_, timeout_, extraHeaders_: <||>] := Module[
  {headers, spec, response, status, payload, cookie},
  headers = <|"Accept" -> "application/json", "Content-Type" -> "application/json",
    "Authorization" -> auth|>;
  headers = Join[headers, extraHeaders];
  If[StringQ[affinity] && affinity =!= "", headers["tkhttp-id"] = affinity];
  spec = <|"Method" -> method, "Headers" -> headers, "Cookies" -> None, "Body" -> body|>;
  response = transport[HTTPRequest[$endpoint <> route, spec], timeout];
  If[!AssociationQ[response], Return[failure["TransportFailure",
    "No complete HTTPS response. Check connectivity and certificate trust. No retry was made; the server outcome may be unknown."]]];
  status = Lookup[response, "StatusCode", Missing["NotAvailable"]];
  If[!IntegerQ[status], Return[failure["InvalidHTTPResponse", "SAS returned no HTTP status."]]];
  If[status == 401, Return[failure["AuthenticationRejected", "SAS rejected authentication. Use your SAS username and password.", <|"HTTPStatus" -> status|>]]];
  If[status == 403, Return[failure["AccessDenied", "SAS denied this operation for the current account.", <|"HTTPStatus" -> status|>]]];
  If[300 <= status < 400, Return[failure["UnexpectedRedirect", "SAS returned a redirect. It was not followed and no retry was made.", <|"HTTPStatus" -> status|>]]];
  If[status < 200 || status >= 300, Return[failure["HTTPFailure", "SAS returned an unsuccessful HTTP status. No retry was made.", <|"HTTPStatus" -> status|>]]];
  payload = parseJSON[Lookup[response, "BodyByteArray", Lookup[response, "Body", ""]]];
  If[!AssociationQ[payload], Return[failure["InvalidJSON", "SAS returned an unexpected response instead of a JSON object.", <|"HTTPStatus" -> status|>]]];
  If[KeyExistsQ[payload, "error"], Return[failure["SASResponseError", "SAS reported an error. No retry or alternate source was used.", <|"HTTPStatus" -> status|>]]];
  cookie = SelectFirst[Replace[Lookup[response, "Cookies", {}], Except[_List] -> {}],
    AssociationQ[#] && Lookup[#, "Name", ""] === "tkhttp-id" &, <||>];
  cookie = Lookup[cookie, "Content", affinity];
  If[!StringQ[cookie] || StringContainsQ[cookie, {"\r", "\n"}], cookie = affinity];
  <|"Payload" -> payload, "Affinity" -> cookie, "HTTPStatus" -> status|>
];

action[name_, parameters_, timeout_] := Module[{answer, payload, disposition, severity},
  answer = request["POST", "cas/sessions/" <> $connection["Session"] <> "/actions/" <> name,
    ExportString[parameters, "RawJSON"], $connection["Authorization"], $connection["Affinity"], timeout];
  If[FailureQ[answer], Return[answer]];
  $connection["Affinity"] = answer["Affinity"];
  payload = answer["Payload"];
  disposition = Lookup[payload, "disposition", <||>];
  If[!AssociationQ[disposition], Return[failure["InvalidDisposition", "SAS returned no valid action status."]]];
  severity = Lookup[<|"Normal" -> 0, "Warning" -> 1, "Error" -> 2|>,
    Lookup[disposition, "severity", ""], Missing["UnknownSeverity"]];
  If[!IntegerQ[severity], Return[failure["UnknownSeverity", "SAS returned an unrecognized action severity."]]];
  If[severity >= 2, Return[failure["CASActionFailed", "The SAS action failed. Check the exact library, path and account access.",
    <|"Severity" -> severity, "HTTPStatus" -> answer["HTTPStatus"]|>]]];
  <|"Results" -> Lookup[payload, "results", <||>], "Severity" -> severity|>
];

credentialsDialog[] := If[$FrontEnd === Null,
  failure["FrontEndRequired", "Run PDSNativeConnect[] in a desktop Wolfram notebook."],
  DialogInput[DynamicModule[{user = "", pass = ""}, Column[{
    Style["Connect to PDS", Bold],
    Grid[{{"SAS Username:", InputField[Dynamic[user], String]},
      {"SAS Password:", InputField[Dynamic[pass], String, FieldMasked -> True]}},
      Alignment -> Left, Spacings -> {1, 1}],
    Row[{Button["Connect", With[{value = {StringTrim[user], pass}},
      user = ""; pass = ""; DialogReturn[value]]], Spacer[15],
      Button["Cancel", user = ""; pass = ""; DialogReturn[$Canceled]]}]
  }, Spacings -> 1]], WindowTitle -> "SEPIA Login"]
];

connectWith[credentials_, timeout_] := Module[{user, pass, auth, answer, session},
  If[!MatchQ[credentials, {_String, _String}], Return[failure["InvalidCredentials", "Enter a SAS username and password."]]];
  {user, pass} = credentials;
  user = StringTrim[user];
  If[user === "" || pass === "" || StringContainsQ[user, ":"], Return[failure["InvalidCredentials", "Enter a nonempty SAS username without a colon and a nonempty password."]]];
  auth = "Basic " <> BaseEncode[ByteArray[ToCharacterCode[user <> ":" <> pass, "UTF8"]]];
  Clear[pass];
  answer = request["PUT", "cas/sessions", "", auth, "", timeout];
  If[FailureQ[answer], Return[answer]];
  session = Lookup[answer["Payload"], "session", Missing["NotAvailable"]];
  If[!validSession[session], Return[failure["InvalidSessionResponse", "SAS returned no recognized session identifier. A server session may exist; do not repeatedly reconnect."]]];
  $connection = <|"Session" -> session, "Authorization" -> auth, "Affinity" -> answer["Affinity"]|>;
  $tables = <||>;
  $listings = <||>;
  uiNewSession[];
  <|"Status" -> "Connected", "AccessMethod" -> "Wolfram_CAS_REST", "Endpoint" -> $endpoint,
    "ConnectorVersion" -> $version, "HTTPStatus" -> answer["HTTPStatus"]|>
];

Options[PDSNativeConnect] = {"Timeout" -> 60, "OpenUI" -> False};
PDSNativeConnect[OptionsPattern[]] := run[Module[{timeout = OptionValue["Timeout"], open = OptionValue["OpenUI"], credentials, answer},
  If[!validTimeout[timeout], Return[failure["InvalidTimeout", "Timeout must be a positive number of seconds, at most 300."]]];
  If[!MemberQ[{True, False}, open], Return[failure["InvalidOption", "OpenUI must be True or False."]]];
  If[$connection =!= None, Return[failure["AlreadyConnected", "Run PDSNativeDisconnect[] before opening another native connection."]]];
  credentials = credentialsDialog[];
  If[FailureQ[credentials], Return[credentials]];
  If[credentials === $Canceled, Return[<|"Status" -> "Canceled"|>]];
  answer = WithCleanup[connectWith[credentials, timeout], Clear[credentials]];
  If[AssociationQ[answer] && TrueQ[open],
    With[{window = Quiet[Check[PDSNativeOpenBrowser[], $Failed]]},
      If[window === $Failed || FailureQ[window], answer = Append[answer, "UIStatus" -> "Connection is active, but the window could not open. Use PDSNativeOpenBrowser[] or PDSNativeDisconnect[]."]]
    ]
  ];
  answer
]];

fileEntries[result_] := Module[{table, schema, names, rows, records},
  If[!AssociationQ[result], Return[failure["InvalidResults", "SAS returned no results object."]]];
  table = Lookup[result, "FileInfo", <||>];
  If[!AssociationQ[table], Return[failure["MissingFileInfo", "SAS returned no file-information table."]]];
  schema = Lookup[table, "schema", Missing["NotAvailable"]];
  rows = Lookup[table, "rows", Missing["NotAvailable"]];
  If[!ListQ[schema] || !AllTrue[schema, AssociationQ] || !ListQ[rows],
    Return[failure["InvalidFileInfo", "The file-information table has an unexpected structure."]]];
  names = Lookup[schema, "name", Missing["NotAvailable"]];
  If[!AllTrue[names, StringQ] || !DuplicateFreeQ[names] || !ContainsAll[names, {"Name", "Size"}] ||
    !AllTrue[rows, ListQ[#] && Length[#] === Length[names] &],
    Return[failure["InvalidFileInfo", "The file-information schema or row widths are inconsistent."]]];
  records = AssociationThread[names, #] & /@ rows;
  If[!AllTrue[records, StringQ[#["Name"]] && NumberQ[#["Size"]] && TrueQ[#["Size"] >= 0] &&
      TrueQ[Round[#["Size"]] == #["Size"]] && StringQ[Lookup[#, "Permission", ""]] &],
    Return[failure["InvalidFileInfo", "SAS returned an invalid file name, size or permission value."]]];
  (<|"Name" -> #["Name"], "Bytes" -> Round[#["Size"]], "Permissions" -> Lookup[#, "Permission", ""]|> &) /@ records
];

Options[PDSNativeListFiles] = {"Timeout" -> 60};
PDSNativeListFiles[caslib_String, directory_String: "", OptionsPattern[]] := run[
  Module[{timeout = OptionValue["Timeout"], parameters, answer, entries},
    If[!validTimeout[timeout], Return[failure["InvalidTimeout", "Timeout must be a positive number of seconds, at most 300."]]];
    If[$connection === None, Return[failure["NotConnected", "Run PDSNativeConnect[] first."]]];
    If[StringTrim[caslib] === "", Return[failure["InvalidCaslib", "Enter the exact SAS library name."]]];
    If[!validPath[directory], Return[failure["InvalidPath", "Use a relative folder path without parent traversal."]]];
    parameters = <|"caslib" -> caslib, "allFiles" -> True|>;
    If[directory =!= "", parameters["path"] = directory];
    answer = action["table.fileInfo", parameters, timeout];
    If[FailureQ[answer], Return[answer]];
    entries = fileEntries[answer["Results"]];
    If[FailureQ[entries], Return[entries]];
    <|"Caslib" -> caslib, "Directory" -> directory, "Entries" -> entries, "Severity" -> answer["Severity"]|>
  ]
];

Options[PDSNativeDisconnect] = {"Timeout" -> 60};
PDSNativeDisconnect[OptionsPattern[]] := run[Module[{timeout = OptionValue["Timeout"], answer, cleanup},
  If[!validTimeout[timeout], Return[failure["InvalidTimeout", "Timeout must be a positive number of seconds, at most 300."]]];
  If[$connection === None, cleanup = zipCleanupResources[];
    If[FailureQ[cleanup], Return[cleanup]]; Return[Join[<|"Status" -> "AlreadyClosed"|>, <|"ZIPCleanup" -> cleanup|>]]];
  answer = action["session.endSession", <||>, timeout];
  If[FailureQ[answer], Return[answer]];
  $connection = None;
  $tables = <||>;
  $listings = <||>;
  cleanup = zipCleanupResources[];
  uiDisconnected[];
  If[FailureQ[cleanup], Return[cleanup]];
  <|"Status" -> "Closed", "Severity" -> answer["Severity"], "ServerCleanup" -> "Confirmed", "ZIPCleanup" -> cleanup|>
]];

tableRecord[token_] := Module[{record},
  If[$connection === None, Return[failure["NotConnected", "Run PDSNativeConnect[] first."]]];
  record = Lookup[$tables, token, Missing["UnknownTable"]];
  If[!AssociationQ[record], Return[failure["UnknownTable", "Use a table token created by this native connection."]]];
  record
];

loadFailure[answer_, record_] := Failure[answer[[1]], Join[answer[[2]],
  <|"Token" -> record["Token"], "TemporaryTable" -> record["TemporaryTable"],
    "Recovery" -> "Run PDSNativeDisconnect[] to end the session and clean up possible temporary tables before reconnecting."|>]];

loadTable[caslib_String, path_String, timeout_] :=
  Module[{token, name, record, answer},
    If[!validTimeout[timeout], Return[failure["InvalidTimeout", "Timeout must be a positive number of seconds, at most 300."]]];
    If[$connection === None, Return[failure["NotConnected", "Run PDSNativeConnect[] first."]]];
    If[StringTrim[caslib] === "", Return[failure["InvalidCaslib", "Enter the exact SAS library name."]]];
    If[path === "" || !validPath[path], Return[failure["InvalidPath", "Use the exact relative file path without parent traversal."]]];
    If[!StringEndsQ[ToLowerCase[path], ".sas7bdat"], Return[failure["UnsupportedFormat", "Only SAS7BDAT files can be loaded."]]];
    If[AnyTrue[Values[$tables], Lookup[#, "State", ""] =!= "Loaded" &],
      Return[failure["CleanupRequired", "An earlier table operation is unresolved. Run PDSNativeDisconnect[] before loading another table."]]];
    token = CreateUUID[];
    name = "sepia_n_" <> StringTake[StringReplace[token, "-" -> ""], 24];
    record = <|"Token" -> token, "Caslib" -> caslib, "RelativePath" -> path,
      "TemporaryTable" -> name, "TemporaryCaslib" -> "CASUSER", "State" -> "LoadUnconfirmed"|>;
    AssociateTo[$tables, token -> record];
    answer = action["table.loadTable", <|"sourceCaslib" -> caslib, "path" -> path,
      "casout" -> <|"name" -> name, "caslib" -> "CASUSER", "replace" -> False, "promote" -> False|>|>, timeout];
    If[FailureQ[answer], Return[loadFailure[answer, record]]];
    record = Join[record, <|"State" -> "Loaded", "LoadSeverity" -> answer["Severity"]|>];
    AssociateTo[$tables, token -> record];
    Join[KeyDrop[record, "State"], <|"Status" -> "Loaded", "LoadStatus" -> If[answer["Severity"] == 0, "Success", "Warning"]|>]
  ];

Options[PDSNativeLoadTable] = {"Timeout" -> 60};
PDSNativeLoadTable[caslib_String, path_String, OptionsPattern[]] := run[loadTable[caslib, path, OptionValue["Timeout"]]];

resultRows[results_, key_, required_] := Module[{table, schema, names, rows},
  If[!AssociationQ[results], Return[failure["InvalidMetadata", "SAS returned no metadata results object."]]];
  table = Lookup[results, key, <||>];
  If[!AssociationQ[table], Return[failure["InvalidMetadata", "SAS returned no expected metadata table."]]];
  schema = Lookup[table, "schema", Missing["NotAvailable"]];
  rows = Lookup[table, "rows", Missing["NotAvailable"]];
  If[!ListQ[schema] || !AllTrue[schema, AssociationQ] || !ListQ[rows],
    Return[failure["InvalidMetadata", "The metadata table has an unexpected structure."]]];
  names = Lookup[schema, "name", Missing["NotAvailable"]];
  If[!AllTrue[names, StringQ] || !DuplicateFreeQ[names] || !ContainsAll[names, required] ||
    !AllTrue[rows, ListQ[#] && Length[#] === Length[names] &],
    Return[failure["InvalidMetadata", "The metadata schema or row widths are inconsistent."]]];
  AssociationThread[names, #] & /@ rows
];

wholeCount[n_] := NumberQ[n] && TrueQ[n >= 0] && TrueQ[n == Round[n]];

inspectTable[token_String, timeout_] :=
  Module[{record, ti, ci, tables, columns, info, rows, count, ids, names},
    If[!validTimeout[timeout], Return[failure["InvalidTimeout", "Timeout must be a positive number of seconds, at most 300."]]];
    record = tableRecord[token];
    If[FailureQ[record], Return[record]];
    If[record["State"] =!= "Loaded", Return[failure["CleanupRequired", "The table operation is unresolved. Run PDSNativeDisconnect[] before reconnecting."]]];
    ti = action["table.tableInfo", <|"caslib" -> "CASUSER", "name" -> record["TemporaryTable"]|>, timeout];
    If[FailureQ[ti], Return[ti]];
    tables = resultRows[ti["Results"], "TableInfo", {"Name", "Rows", "Columns"}];
    If[FailureQ[tables], Return[tables]];
    If[Length[tables] =!= 1, Return[failure["TableMismatch", "SAS returned no unique metadata row for this temporary table."]]];
    info = First[tables];
    If[!StringQ[info["Name"]] || ToUpperCase[info["Name"]] =!= ToUpperCase[record["TemporaryTable"]],
      Return[failure["TableMismatch", "SAS returned metadata for an unexpected table."]]];
    If[KeyExistsQ[info, "Global"] && !(TrueQ[info["Global"] == 0] || info["Global"] === False),
      Return[failure["UnexpectedTableScope", "The returned table is not confirmed as session-scoped."]]];
    {rows, count} = Lookup[info, {"Rows", "Columns"}];
    If[!wholeCount[rows] || !wholeCount[count], Return[failure["InvalidDimensions", "SAS returned invalid table dimensions."]]];
    {rows, count} = Round[{rows, count}];
    ci = action["table.columnInfo", <|"table" -> <|"name" -> record["TemporaryTable"], "caslib" -> "CASUSER"|>|>, timeout];
    If[FailureQ[ci], Return[ci]];
    columns = resultRows[ci["Results"], "ColumnInfo", {"Column", "ID"}];
    If[FailureQ[columns], Return[columns]];
    ids = Lookup[columns, "ID"];
    If[Length[columns] =!= count || !AllTrue[ids, wholeCount] || Sort[ids] != Range[count],
      Return[failure["ColumnMismatch", "The column count or column positions do not match the table metadata."]]];
    columns = SortBy[columns, #["ID"] &];
    names = Lookup[columns, "Column"];
    If[!AllTrue[names, StringQ[#] && # =!= "" &] || !DuplicateFreeQ[names],
      Return[failure["ColumnMismatch", "Column names must be nonempty, unique strings."]]];
    <|"SourceFile" -> record["RelativePath"], "Dimensions" -> {rows, count}, "ColumnNames" -> names,
      "LoadSeverity" -> record["LoadSeverity"], "LoadStatus" -> If[record["LoadSeverity"] == 0, "Success", "Warning"],
      "Severity" -> Max[record["LoadSeverity"], ti["Severity"], ci["Severity"]],
      "MetadataStatus" -> "Retrieved", "ColumnMetadata" -> columns,
      "Provenance" -> KeyDrop[record, "State"]|>
  ];

Options[PDSNativeInspectTable] = {"Timeout" -> 60};
PDSNativeInspectTable[token_String, OptionsPattern[]] := run[inspectTable[token, OptionValue["Timeout"]]];

Options[PDSNativeDropTable] = {"Timeout" -> 60};
PDSNativeDropTable[token_String, OptionsPattern[]] := run[
  Module[{timeout = OptionValue["Timeout"], record, answer},
    If[!validTimeout[timeout], Return[failure["InvalidTimeout", "Timeout must be a positive number of seconds, at most 300."]]];
    record = tableRecord[token];
    If[FailureQ[record], Return[record]];
    If[record["State"] =!= "Loaded", Return[failure["CleanupRequired", "The table operation is unresolved. Run PDSNativeDisconnect[] to clean up the session."]]];
    AssociateTo[$tables, token -> Join[record, <|"State" -> "DropUnconfirmed"|>]];
    answer = action["table.dropTable", <|"name" -> record["TemporaryTable"], "caslib" -> "CASUSER", "quiet" -> False|>, timeout];
    If[FailureQ[answer], Return[loadFailure[answer, record]]];
    KeyDropFrom[$tables, token];
    <|"Status" -> "Dropped", "Severity" -> answer["Severity"]|>
  ]
];

temporalKind[type_, format_] := Which[
  MemberQ[{"date", "datetime", "time"}, type], type,
  type =!= "double", None,
  StringMatchQ[ToUpperCase[format], RegularExpression["(?:DATETIME|DATEAMPM|E8601DT|B8601DT|E8601DZ|B8601DZ)[0-9]*(?:\\.[0-9]*)?"]], "sasdatetime",
  StringMatchQ[ToUpperCase[format], RegularExpression["(?:DATE|YYMMDD[A-Z]?|MMDDYY[A-Z]?|DDMMYY[A-Z]?|E8601DA|B8601DA|JULIAN)[0-9]*(?:\\.[0-9]*)?"]], "sasdate",
  StringMatchQ[ToUpperCase[format], RegularExpression["(?:TIME|TIMEAMPM|TOD|HHMM)[0-9]*(?:\\.[0-9]*)?"]], "sastime",
  True, None
];

dateText[days_Integer] := Quiet[Check[
  DateString[FromAbsoluteTime[
    AbsoluteTime[DateObject[{1960, 1, 1, 0, 0, 0}, TimeZone -> 0]] + 86400 days,
    TimeZone -> 0], {"Year", "-", "Month", "-", "Day"}], $Failed]];

timeText[microseconds_Integer] := Module[{seconds, fraction},
  seconds = Quotient[microseconds, 1000000]; fraction = Mod[microseconds, 1000000];
  StringRiffle[IntegerString[#, 10, 2] & /@ {
    Quotient[seconds, 3600], Quotient[Mod[seconds, 3600], 60], Mod[seconds, 60]}, ":"] <>
    If[fraction == 0, "", "." <> IntegerString[fraction, 10, 6]]
];

decodeCell[value_, column_Association] := Module[{type, format, kind, units, days, text, fail},
  fail[] := Throw[failure["UnsupportedCell", "A cell does not match its supported CAS type; no partial data were returned.",
    <|"Column" -> column["name"], "CASType" -> Lookup[column, "type", Missing["NotAvailable"]]|>], "SEPIANativeCell"];
  type = ToLowerCase[Lookup[column, "type", ""]];
  format = Lookup[column, "format", ""];
  If[format === Null, format = ""];
  If[!StringQ[format], fail[]];
  If[!MemberQ[{"string", "char", "varchar", "double", "float", "int", "int32", "int64", "date", "datetime", "time", "boolean"}, type], fail[]];
  If[value === Null, Return[Missing["CASMissing"]]];
  If[MemberQ[{"int32", "date"}, type] && TrueQ[value == -2147483648], Return[Missing["CASMissing"]]];
  If[MemberQ[{"int", "int64", "datetime", "time"}, type] && TrueQ[value == -9223372036854775808], Return[Missing["CASMissing"]]];
  If[MemberQ[{"string", "char", "varchar"}, type], If[!StringQ[value], fail[]]; Return[value]];
  If[type === "boolean", If[!MemberQ[{True, False}, value], fail[]]; Return[value]];
  If[!MatchQ[value, _Integer | _Real], fail[]];
  If[MemberQ[{"int", "int32", "int64", "date", "datetime", "time"}, type] && !IntegerQ[value], fail[]];
  kind = temporalKind[type, format];
  If[kind === None, Return[value]];
  If[MemberQ[{"date", "sasdate"}, kind],
    If[!TrueQ[value == Round[value]], fail[]];
    text = dateText[Round[value]];
    If[!StringQ[text] || !StringMatchQ[text, RegularExpression["[0-9]{4}-[0-9]{2}-[0-9]{2}"]], fail[]];
    Return[<|"SEPIAType" -> "Date", "ISO8601" -> text|>]
  ];
  units = If[StringStartsQ[kind, "sas"], value 1000000, value];
  If[!TrueQ[units == Round[units]], fail[]];
  units = Round[units];
  If[MemberQ[{"time", "sastime"}, kind],
    If[!TrueQ[0 <= units < 86400000000], fail[]];
    Return[<|"SEPIAType" -> "Time", "ISO8601" -> timeText[units]|>]
  ];
  days = Quotient[units, 86400000000];
  text = dateText[days];
  If[!StringQ[text] || !StringMatchQ[text, RegularExpression["[0-9]{4}-[0-9]{2}-[0-9]{2}"]], fail[]];
  <|"SEPIAType" -> "DateTime", "ISO8601" -> text <> "T" <> timeText[Mod[units, 86400000000]]|>
];

decodeFetch[results_, metadata_] := Module[{table, schema, names, rows, wanted, order, selected, types, data},
  If[!AssociationQ[results], Return[failure["InvalidFetch", "SAS returned no fetch results object."]]];
  table = Lookup[results, "Fetch", <||>];
  If[!AssociationQ[table], Return[failure["InvalidFetch", "SAS returned no Fetch table."]]];
  schema = Lookup[table, "schema", Missing["NotAvailable"]];
  rows = Lookup[table, "rows", Missing["NotAvailable"]];
  If[!ListQ[schema] || !AllTrue[schema, AssociationQ] || !ListQ[rows],
    Return[failure["InvalidFetch", "The Fetch table has an unexpected structure."]]];
  names = Lookup[schema, "name", Missing["NotAvailable"]];
  types = Lookup[schema, "type", Missing["NotAvailable"]];
  wanted = metadata["ColumnNames"];
  If[!AllTrue[names, StringQ] || !DuplicateFreeQ[names] || Sort[names] =!= Sort[wanted] || !AllTrue[types, StringQ],
    Return[failure["ColumnMismatch", "Fetched columns do not match the inspected source schema."]]];
  If[Length[rows] =!= metadata["Dimensions"][[1]] || !AllTrue[rows, ListQ[#] && Length[#] === Length[names] &],
    Return[failure["IncompleteFetch", "Fetched rows or row widths do not match the complete table; no partial data were returned."]]];
  order = First[FirstPosition[names, #]] & /@ wanted;
  selected = schema[[order]];
  data = Catch[(MapThread[decodeCell, {#[[order]], selected}] &) /@ rows, "SEPIANativeCell"];
  If[FailureQ[data], Return[data]];
  <|"Data" -> data, "WireSchema" -> selected|>
];

Options[PDSNativeFetchTable] = {"MaxRows" -> 1000, "MaxCells" -> 200000, "Timeout" -> 60};
PDSNativeFetchTable[token_String, OptionsPattern[]] := run[
  Module[{maxRows = OptionValue["MaxRows"], maxCells = OptionValue["MaxCells"],
    timeout = OptionValue["Timeout"], record, metadata, rows, columns, fetched, decoded, severity},
    If[!IntegerQ[maxRows] || !IntegerQ[maxCells] || maxRows < 1 || maxCells < 1,
      Return[failure["InvalidLimit", "MaxRows and MaxCells must be positive integers."]]];
    If[!validTimeout[timeout], Return[failure["InvalidTimeout", "Timeout must be a positive number of seconds, at most 300."]]];
    record = tableRecord[token];
    If[FailureQ[record], Return[record]];
    metadata = inspectTable[token, timeout];
    If[FailureQ[metadata], Return[metadata]];
    {rows, columns} = metadata["Dimensions"];
    If[rows > maxRows || rows columns > maxCells, Return[failure["FetchLimit", "The table exceeds the requested limits; no patient rows were fetched.",
      <|"Dimensions" -> {rows, columns}, "MaxRows" -> maxRows, "MaxCells" -> maxCells|>]]];
    severity = metadata["Severity"];
    If[rows == 0, decoded = <|"Data" -> {}, "WireSchema" -> Missing["NoRowsFetched"]|>,
      fetched = action["table.fetch", <|"table" -> <|"name" -> record["TemporaryTable"], "caslib" -> "CASUSER"|>,
        "from" -> 1, "to" -> rows, "maxRows" -> rows, "index" -> False, "format" -> False, "sasTypes" -> False|>, timeout];
      If[FailureQ[fetched], Return[fetched]];
      decoded = decodeFetch[fetched["Results"], metadata];
      If[FailureQ[decoded], Return[decoded]];
      severity = Max[severity, fetched["Severity"]]
    ];
    <|"Dimensions" -> {rows, columns}, "ColumnNames" -> metadata["ColumnNames"], "Data" -> decoded["Data"],
      "WireSchema" -> decoded["WireSchema"], "ColumnMetadata" -> metadata["ColumnMetadata"],
      "Severity" -> severity, "Provenance" -> Join[metadata["Provenance"],
        <|"Endpoint" -> $endpoint, "AccessMethod" -> "Wolfram_CAS_REST", "ConnectorVersion" -> $version,
          "FetchedAtUTC" -> DateString[Now, {"ISODateTime"}, TimeZone -> 0],
          "OriginalFileSHA256" -> Missing["NotComputed"], "RowOrder" -> "Unspecified"|>],
      "Warnings" -> {"CAS results are not a lossless replacement for the original SAS file; special missing tags may be collapsed.",
        "Strings retain JSON whitespace; SWAT may trim trailing whitespace.",
        "Dates and times use tagged ISO8601 values based on CAS types or recognized SAS formats; the source time zone is not established.",
        "Compare records by verified keys, not row positions."}|>
  ]
];

validFetched[result_] := AssociationQ[result] && ListQ[Lookup[result, "ColumnNames", None]] &&
  AllTrue[result["ColumnNames"], StringQ] && DuplicateFreeQ[result["ColumnNames"]] &&
  ListQ[Lookup[result, "Data", None]] && AllTrue[result["Data"], ListQ[#] && Length[#] === Length[result["ColumnNames"]] &];

sameCell[a_, b_] := a === b || (MatchQ[a, _Integer | _Real] && MatchQ[b, _Integer | _Real] && Rationalize[a, 0] === Rationalize[b, 0]);

PDSNativeCompareTables[first_, second_, key_String] := Module[
  {names, order, a, b, keyIndex, idsA, idsB, index, aligned, counts},
  If[!validFetched[first] || !validFetched[second], Return[failure["InvalidComparison", "Supply two successful complete fetch results."]]];
  names = first["ColumnNames"];
  If[Sort[names] =!= Sort[second["ColumnNames"]] || !MemberQ[names, key],
    Return[failure["ComparisonSchemaMismatch", "The results must contain the same columns and the specified record key."]]];
  order = First[FirstPosition[second["ColumnNames"], #]] & /@ names;
  a = first["Data"]; b = (#[[order]] &) /@ second["Data"];
  keyIndex = First[FirstPosition[names, key]];
  idsA = If[a === {}, {}, a[[All, keyIndex]]]; idsB = If[b === {}, {}, b[[All, keyIndex]]];
  If[!AllTrue[Join[idsA, idsB], StringQ[#] && StringTrim[#] =!= "" &] || !DuplicateFreeQ[idsA] || !DuplicateFreeQ[idsB],
    Return[failure["InvalidComparisonKey", "Comparison requires unique, populated string keys in both results."]]];
  If[Sort[idsA] =!= Sort[idsB], Return[<|"KeySetsMatch" -> False, "FirstRows" -> Length[a], "SecondRows" -> Length[b],
    "OnlyInFirstCount" -> Length[Complement[idsA, idsB]], "OnlyInSecondCount" -> Length[Complement[idsB, idsA]],
    "AllValuesMatch" -> False|>]];
  index = AssociationThread[idsB, Range[Length[idsB]]];
  aligned = b[[Lookup[index, idsA]]];
  counts = AssociationThread[names, Table[Count[MapThread[sameCell, {If[a === {}, {}, a[[All, i]]],
      If[aligned === {}, {}, aligned[[All, i]]]}], False], {i, Length[names]}]];
  <|"RowsCompared" -> Length[a], "ColumnsCompared" -> Length[names], "KeySetsMatch" -> True,
    "ColumnOrderMatches" -> (names === second["ColumnNames"]), "MismatchedCellsByColumn" -> Select[counts, # > 0 &],
    "AllValuesMatch" -> (Total[Values[counts]] == 0)|>
];

browseContribution[caslib_String, directory_String, timeout_] := Module[
  {answer, entries, path, normalized, token, listing, name, full, kind},
  If[!validTimeout[timeout], Return[failure["InvalidTimeout", "Timeout must be a positive number of seconds, at most 300."]]];
  If[$connection === None, Return[failure["NotConnected", "Run PDSNativeConnect[] first."]]];
  If[StringTrim[caslib] === "", Return[failure["InvalidCaslib", "Enter the exact SAS library name."]]];
  If[!validPath[directory], Return[failure["InvalidPath", "Use a relative folder path without parent traversal."]]];
  path = StringTrim[directory, "/"];
  answer = action["table.fileInfo", Join[<|"caslib" -> caslib, "allFiles" -> True|>,
    If[path === "", <||>, <|"path" -> path|>]], timeout];
  If[FailureQ[answer], Return[answer]];
  entries = fileEntries[answer["Results"]];
  If[FailureQ[entries], Return[entries]];
  normalized = {};
  normalized = Catch[Do[
    name = StringTrim[entry["Name"], "/"];
    If[MemberQ[{".", ".."}, name], Continue[]];
    If[name === "" || !validPath[entry["Name"]],
      Throw[failure["InvalidListingPath", "SAS returned an unsafe entry path; no path was guessed."], "SEPIANativeListing"]];
    full = Which[
      !StringContainsQ[name, "/"], If[path === "", "", path <> "/"] <> name,
      path === "" || StringStartsQ[name, path <> "/"], name,
      True, Throw[failure["AmbiguousListingPath", "SAS returned an ambiguous entry path; no path was guessed."], "SEPIANativeListing"]
    ];
    kind = Which[StringStartsQ[entry["Permissions"], "d"], "Directory",
      StringStartsQ[entry["Permissions"], "-"], "File", True, "Unknown"];
    AppendTo[normalized, Join[entry, <|"Index" -> Length[normalized] + 1, "RelativePath" -> full,
      "Kind" -> kind, "SupportedImport" -> (kind === "File" && StringEndsQ[ToLowerCase[full], ".sas7bdat"])|>]],
    {entry, entries}
  ]; normalized, "SEPIANativeListing"];
  If[FailureQ[normalized], Return[normalized]];
  token = CreateUUID[];
  listing = <|"ListingToken" -> token, "ContributionID" -> caslib, "Caslib" -> caslib,
    "IdentityStatus" -> "UserSuppliedNotVerified", "Directory" -> path,
    "ParentDirectory" -> If[StringContainsQ[path, "/"], StringRiffle[Most[StringSplit[path, "/"]], "/"], ""],
    "Entries" -> normalized, "Severity" -> answer["Severity"]|>;
  AssociateTo[$listings, token -> listing];
  listing
];

Options[PDSNativeBrowseContribution] = {"Timeout" -> 60};
PDSNativeBrowseContribution[caslib_String, directory_String: "", OptionsPattern[]] :=
  run[browseContribution[caslib, directory, OptionValue["Timeout"]]];

Options[PDSNativeOpenEntry] = {"Timeout" -> 60};
PDSNativeOpenEntry[supplied_Association, index_Integer, OptionsPattern[]] := run[
  Module[{timeout = OptionValue["Timeout"], token, listing, entry, loaded, identity},
    If[!validTimeout[timeout], Return[failure["InvalidTimeout", "Timeout must be a positive number of seconds, at most 300."]]];
    If[$connection === None, Return[failure["NotConnected", "Run PDSNativeConnect[] first."]]];
    token = Lookup[supplied, "ListingToken", None];
    If[!StringQ[token] || !KeyExistsQ[$listings, token],
      Return[failure["UnknownListing", "Browse again in the current native connection."]]];
    listing = $listings[token];
    If[index < 1 || index > Length[listing["Entries"]], Return[failure["InvalidSelection", "Choose an Index from the current file listing."]]];
    entry = listing["Entries"][[index]];
    If[entry["Kind"] === "Directory", Return[browseContribution[listing["Caslib"], entry["RelativePath"], timeout]]];
    If[!TrueQ[entry["SupportedImport"]], Return[failure["UnsupportedEntry", "Open folders or load regular SAS7BDAT files. Archives and documents are not imported."]]];
    loaded = loadTable[listing["Caslib"], entry["RelativePath"], timeout];
    If[FailureQ[loaded], Return[loaded]];
    identity = <|"ContributionID" -> listing["ContributionID"], "IdentityStatus" -> "UserSuppliedNotVerified"|>;
    AssociateTo[$tables, loaded["Token"] -> Join[$tables[loaded["Token"]], identity]];
    Join[loaded, identity]
  ]
];
PDSNativeOpenEntry[_Association, Except[_Integer], OptionsPattern[]] :=
  failure["InvalidSelection", "Choose an integer Index from the current file listing."];

Get[FileNameJoin[{DirectoryName[$InputFileName], "SEPIANativeBrowser.wl"}]];

End[];
EndPackage[];

Get[FileNameJoin[{DirectoryName[$InputFileName], "SEPIANativeUpload.wl"}]];
Get[FileNameJoin[{DirectoryName[$InputFileName], "SEPIAZIPProbe.wl"}]];
Get[FileNameJoin[{DirectoryName[$InputFileName], "SEPIANativeZIP.wl"}]];
