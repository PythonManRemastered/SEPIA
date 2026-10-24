import datetime
import unittest
from types import SimpleNamespace
from unittest.mock import Mock
import pandas as pd
from sepia_pds_bridge import Bridge, RemoteError, scalar


class Result(dict):
    def __init__(self, severity=0, **items):
        super().__init__(items)
        self.severity = severity


class BridgeTests(unittest.TestCase):
    def setUp(self):
        self.b = Bridge()
        self.frame = pd.DataFrame({"ID": ["a", "b"], "VALUE": [1.0, float("nan")]})
        self.table = Mock()
        self.table.shape = (2, 2)
        self.table.columns = ["ID", "VALUE"]
        self.table.fetch.return_value = Result(Fetch=self.frame)
        self.conn = Mock()
        self.conn.CASTable.return_value = self.table
        self.conn.table.loadTable.return_value = Result()
        self.conn.table.dropTable.return_value = Result()
        self.b.connection = self.conn

    def load(self):
        return self.b.dispatch("load", ["study", "folder/adsl.sas7bdat"])

    def test_load_and_provenance(self):
        loaded = self.load()
        self.assertTrue(loaded["OK"])
        fetched = self.b.dispatch("fetch", [loaded["Token"], 10, 100])
        self.assertTrue(fetched["OK"])
        self.assertEqual(fetched["Dimensions"], [2, 2])
        self.assertEqual(fetched["Data"][1][1], {"SEPIAType": "Missing", "Reason": "CASMissing"})
        self.assertEqual(fetched["Provenance"]["RelativePath"], "folder/adsl.sas7bdat")
        args = self.conn.table.loadTable.call_args.kwargs
        self.assertFalse(args["casout"]["replace"])
        self.assertFalse(args["casout"]["promote"])

    def test_cas_severity_not_exception(self):
        self.conn.table.loadTable.return_value = Result(severity=2)
        self.assertFalse(self.load()["OK"])
        self.assertEqual(self.b.tables, {})

    def test_warning_retained(self):
        self.conn.table.loadTable.return_value = Result(severity=1)
        self.assertEqual(self.load()["LoadSeverity"], 1)

    def test_fetch_limit_prevents_transfer(self):
        token = self.load()["Token"]
        result = self.b.dispatch("fetch", [token, 1, 100])
        self.assertEqual(result["Code"], "FetchLimit")
        self.table.fetch.assert_not_called()

    def test_cell_limit_prevents_transfer(self):
        token = self.load()["Token"]
        self.assertEqual(self.b.dispatch("fetch", [token, 10, 3])["Code"], "FetchLimit")
        self.table.fetch.assert_not_called()

    def test_truncated_fetch_rejected(self):
        token = self.load()["Token"]
        self.table.fetch.return_value = Result(Fetch=self.frame.iloc[:1])
        result = self.b.dispatch("fetch", [token])
        self.assertEqual(result["Code"], "IncompleteFetch")
        self.assertNotIn("Data", result)

    def test_fetch_error_rejected(self):
        token = self.load()["Token"]
        self.table.fetch.return_value = Result(severity=2)
        self.assertEqual(self.b.dispatch("fetch", [token])["Code"], "CASActionFailed")

    def test_foreign_token_cannot_drop(self):
        self.assertEqual(self.b.dispatch("drop", ["other"])["Code"], "UnknownTable")
        self.conn.table.dropTable.assert_not_called()

    def test_drop_invalidates_token(self):
        token = self.load()["Token"]
        self.assertTrue(self.b.dispatch("drop", [token])["OK"])
        self.assertEqual(self.b.dispatch("fetch", [token])["Code"], "UnknownTable")

    def test_failed_drop_keeps_tracking(self):
        token = self.load()["Token"]
        self.conn.table.dropTable.return_value = Result(severity=2)
        self.assertFalse(self.b.dispatch("drop", [token])["OK"])
        self.assertIn(token, self.b.tables)

    def test_traversal_and_format_rejected(self):
        for path in ["../adsl.sas7bdat", "/adsl.sas7bdat", "a/../adsl.sas7bdat", "a\\b.sas7bdat", "data.zip"]:
            self.assertFalse(self.b.dispatch("load", ["study", path])["OK"])
        self.conn.table.loadTable.assert_not_called()

    def test_file_names_not_truncated(self):
        name = "A" * 100 + ".sas7bdat"
        self.conn.table.fileInfo.return_value = Result(FileInfo=pd.DataFrame([
            {"Name": name, "Size": 123, "Permission": "-rw-r--r--"}]))
        result = self.b.dispatch("files", ["study", "folder"])
        self.assertEqual(result["Entries"][0]["Name"], name)

    def test_exception_does_not_leak(self):
        self.conn.table.fileInfo.side_effect = ValueError("secret-password patient-value")
        result = str(self.b.dispatch("files", ["study"]))
        self.assertNotIn("secret-password", result)
        self.assertNotIn("patient-value", result)

    def test_scalars_preserve_blank_and_tag_dates(self):
        self.assertEqual(scalar(""), "")
        self.assertEqual(scalar(datetime.date(2020, 1, 2)),
                         {"SEPIAType": "Date", "ISO8601": "2020-01-02"})
        self.assertEqual(scalar(pd.NaT)["SEPIAType"], "Missing")
        with self.assertRaises(RemoteError):
            scalar(float("inf"))

    def test_close_clears_tables(self):
        self.load()
        self.assertTrue(self.b.dispatch("close", [])["OK"])
        self.assertIsNone(self.b.connection)
        self.assertEqual(self.b.tables, {})
        self.assertEqual(self.b.dispatch("files", ["study"])["Code"], "NotConnected")

    def test_generated_fetch_index_not_source_data(self):
        token = self.load()["Token"]
        frame = self.frame.copy()
        frame.insert(0, "_Index_", [1, 2])
        self.table.fetch.return_value = Result(Fetch=frame)
        result = self.b.dispatch("fetch", [token])
        self.assertEqual(result["ColumnNames"], ["ID", "VALUE"])
        self.assertEqual(len(result["Data"][0]), 2)


if __name__ == "__main__":
    unittest.main()
