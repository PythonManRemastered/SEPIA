Get[FileNameJoin[{DirectoryName[$InputFileName], "SEPIANative.wl"}]];
Begin["SEPIANative`Private`"];
Global`SEPIANativeTableTestReport = Module[{response, success, table, requests, modes, name,
    setup, mock, loaded, metadata, dropped, second, tests},
  response[status_, payload_] := <|"StatusCode" -> status, "Body" -> ExportString[payload, "RawJSON"], "Cookies" -> {}|>;
  success[results_: <||>, severity_: "Normal"] := response[200, <|"disposition" -> <|"severity" -> severity|>, "results" -> results|>];
  table[columns_, rows_] := <|"_ctb" -> True, "schema" -> (<|"name" -> #|> & /@ columns), "rows" -> rows|>;
  setup[] := ($connection = <|"Session" -> "11111111-2222-3333-4444-555555555555",
    "Authorization" -> "Basic synthetic", "Affinity" -> ""|>; $tables = <||>; $busy = False;
    requests = {}; modes = <||>; name = "";);
  mock[req_] := Module[{op, params, mode, rows, cols},
    AppendTo[requests, req];
    op = Last[StringSplit[req["URL"], "/"]];
    params = ImportString[req["Body"], "RawJSON"];
    mode = Lookup[modes, op, "Normal"];
    If[mode === "TransportFailure", Return[$Failed]];
    If[mode === "Abort", Abort[]];
    If[mode === "Error", Return[success[<||>, "Error"]]];
    Switch[op,
      "table.loadTable", name = params["casout"]["name"]; success[<||>, mode],
      "table.tableInfo",
        rows = {{If[mode === "WrongName", "another_table", name], 385, 2, If[mode === "Global", 1, 0]}};
        If[mode === "WrongCount", rows[[1, 3]] = 3];
        If[mode === "InvalidRows", rows[[1, 2]] = -1];
        success[<|"TableInfo" -> table[{"Name", "Rows", "Columns", "Global"}, rows]|>, If[mode === "Warning", "Warning", "Normal"]],
      "table.columnInfo",
        cols = {{"AGE", 2}, {"USUBJID", 1}};
        If[mode === "DuplicateNames", cols[[1, 1]] = "USUBJID"];
        If[mode === "DuplicateIDs", cols[[1, 2]] = 1];
        If[mode === "MissingColumn", cols = Rest[cols]];
        success[<|"ColumnInfo" -> table[{"Column", "ID"}, cols]|>, If[mode === "Warning", "Warning", "Normal"]],
      "table.dropTable" | "session.endSession", success[],
      _, response[400, <|"error" -> "Unexpected action"|>]
    ]
  ];
  tests = Block[{$connection, $tables, $busy, transport},
    transport[req_, timeout_] := mock[req];
    {
      VerificationTest[setup[]; loaded = PDSNativeLoadTable["SYNTHETIC", "folder/adsl.sas7bdat"];
        {loaded["Status"], loaded["TemporaryCaslib"], loaded["LoadSeverity"], Length[$tables]},
        {"Loaded", "CASUSER", 0, 1}, TestID -> "native-table-load"],
      VerificationTest[setup[]; loaded = PDSNativeLoadTable["SYNTHETIC", "folder/adsl.sas7bdat"];
        ImportString[First[requests]["Body"], "RawJSON"],
        <|"sourceCaslib" -> "SYNTHETIC", "path" -> "folder/adsl.sas7bdat",
          "casout" -> <|"name" -> name, "caslib" -> "CASUSER", "replace" -> False, "promote" -> False|>|>,
        TestID -> "load-does-not-replace-or-promote"],
      VerificationTest[setup[]; loaded = PDSNativeLoadTable["SYNTHETIC", "adsl.sas7bdat"];
        second = PDSNativeLoadTable["SYNTHETIC", "adsl.sas7bdat"];
        {loaded["Token"] =!= second["Token"], loaded["TemporaryTable"] =!= second["TemporaryTable"],
          StringLength[loaded["TemporaryTable"]]}, {True, True, 32}, TestID -> "unique-bounded-table-names"],
      VerificationTest[setup[]; {PDSNativeLoadTable["SYNTHETIC", "x.pdf"][[1]], requests},
        {"UnsupportedFormat", {}}, TestID -> "unsupported-load-rejected"],
      VerificationTest[setup[]; {PDSNativeLoadTable["SYNTHETIC", "../adsl.sas7bdat"][[1]], requests},
        {"InvalidPath", {}}, TestID -> "unsafe-load-rejected"],
      VerificationTest[setup[]; {PDSNativeLoadTable["SYNTHETIC", "adsl.sas7bdat", "Timeout" -> 0][[1]], requests},
        {"InvalidTimeout", {}}, TestID -> "load-timeout-validation"],
      VerificationTest[setup[]; loaded = PDSNativeLoadTable["SYNTHETIC", "adsl.sas7bdat"];
        metadata = PDSNativeInspectTable[loaded["Token"]];
        {metadata["Dimensions"], metadata["ColumnNames"], metadata["Severity"],
          Last[StringSplit[#["URL"], "/"]] & /@ requests},
        {{385, 2}, {"USUBJID", "AGE"}, 0, {"table.loadTable", "table.tableInfo", "table.columnInfo"}},
        TestID -> "inspection-orders-columns-without-fetching-records"],
      VerificationTest[setup[]; loaded = PDSNativeLoadTable["SYNTHETIC", "adsl.sas7bdat"];
        modes["table.loadTable"] = "Warning"; second = PDSNativeLoadTable["SYNTHETIC", "adsl.sas7bdat"];
        metadata = PDSNativeInspectTable[second["Token"]]; {metadata["LoadStatus"], metadata["Severity"]},
        {"Warning", 1}, TestID -> "load-warning-preserved"],
      VerificationTest[setup[]; loaded = PDSNativeLoadTable["SYNTHETIC", "adsl.sas7bdat"];
        modes["table.columnInfo"] = "Warning"; PDSNativeInspectTable[loaded["Token"]]["Severity"],
        1, TestID -> "metadata-warning-preserved"],
      VerificationTest[setup[]; loaded = PDSNativeLoadTable["SYNTHETIC", "adsl.sas7bdat"];
        modes["table.tableInfo"] = "WrongName"; PDSNativeInspectTable[loaded["Token"]][[1]],
        "TableMismatch", TestID -> "wrong-table-rejected"],
      VerificationTest[setup[]; loaded = PDSNativeLoadTable["SYNTHETIC", "adsl.sas7bdat"];
        modes["table.tableInfo"] = "Global"; PDSNativeInspectTable[loaded["Token"]][[1]],
        "UnexpectedTableScope", TestID -> "global-table-rejected"],
      VerificationTest[setup[]; loaded = PDSNativeLoadTable["SYNTHETIC", "adsl.sas7bdat"];
        modes["table.tableInfo"] = "InvalidRows"; PDSNativeInspectTable[loaded["Token"]][[1]],
        "InvalidDimensions", TestID -> "invalid-row-count"],
      VerificationTest[setup[]; loaded = PDSNativeLoadTable["SYNTHETIC", "adsl.sas7bdat"];
        modes["table.tableInfo"] = "WrongCount"; PDSNativeInspectTable[loaded["Token"]][[1]],
        "ColumnMismatch", TestID -> "inconsistent-column-count"],
      VerificationTest[setup[]; loaded = PDSNativeLoadTable["SYNTHETIC", "adsl.sas7bdat"];
        modes["table.columnInfo"] = "DuplicateNames"; PDSNativeInspectTable[loaded["Token"]][[1]],
        "ColumnMismatch", TestID -> "duplicate-column-names"],
      VerificationTest[setup[]; loaded = PDSNativeLoadTable["SYNTHETIC", "adsl.sas7bdat"];
        modes["table.columnInfo"] = "DuplicateIDs"; PDSNativeInspectTable[loaded["Token"]][[1]],
        "ColumnMismatch", TestID -> "duplicate-column-positions"],
      VerificationTest[setup[]; loaded = PDSNativeLoadTable["SYNTHETIC", "adsl.sas7bdat"];
        modes["table.columnInfo"] = "MissingColumn"; PDSNativeInspectTable[loaded["Token"]][[1]],
        "ColumnMismatch", TestID -> "incomplete-column-metadata"],
      VerificationTest[setup[]; {PDSNativeInspectTable["foreign"][[1]], PDSNativeDropTable["foreign"][[1]], requests},
        {"UnknownTable", "UnknownTable", {}}, TestID -> "unowned-table-tokens-rejected"],
      VerificationTest[setup[]; loaded = PDSNativeLoadTable["SYNTHETIC", "adsl.sas7bdat"];
        modes["table.columnInfo"] = "Error"; metadata = PDSNativeInspectTable[loaded["Token"]];
        dropped = PDSNativeDropTable[loaded["Token"]];
        {metadata[[1]], dropped["Status"], Length[$tables]}, {"CASActionFailed", "Dropped", 0},
        TestID -> "failed-inspection-keeps-cleanup-token"],
      VerificationTest[setup[]; modes["table.loadTable"] = "TransportFailure";
        loaded = PDSNativeLoadTable["SYNTHETIC", "adsl.sas7bdat"];
        {loaded[[1]], StringQ[loaded[[2]]["Token"]], Length[$tables],
          PDSNativeLoadTable["SYNTHETIC", "adsl.sas7bdat"][[1]], Length[requests]},
        {"TransportFailure", True, 1, "CleanupRequired", 1}, TestID -> "uncertain-load-recorded-and-not-retried"],
      VerificationTest[setup[]; modes["table.loadTable"] = "Error";
        loaded = PDSNativeLoadTable["SYNTHETIC", "adsl.sas7bdat"];
        {loaded[[1]], PDSNativeDropTable[loaded[[2]]["Token"]][[1]], Length[requests]},
        {"CASActionFailed", "CleanupRequired", 1}, TestID -> "failed-load-cannot-drop-preexisting-table"],
      VerificationTest[setup[]; modes["table.loadTable"] = "Abort";
        CheckAbort[PDSNativeLoadTable["SYNTHETIC", "adsl.sas7bdat"], Null];
        {$busy, Length[$tables], First[Values[$tables]]["State"]}, {False, 1, "LoadUnconfirmed"},
        TestID -> "aborted-load-retains-cleanup-record"],
      VerificationTest[setup[]; loaded = PDSNativeLoadTable["SYNTHETIC", "adsl.sas7bdat"];
        modes["table.dropTable"] = "TransportFailure";
        {PDSNativeDropTable[loaded["Token"]][[1]], Length[$tables],
          PDSNativeLoadTable["SYNTHETIC", "adsl.sas7bdat"][[1]]},
        {"TransportFailure", 1, "CleanupRequired"}, TestID -> "uncertain-drop-blocks-new-loads"],
      VerificationTest[setup[]; loaded = PDSNativeLoadTable["SYNTHETIC", "adsl.sas7bdat"];
        PDSNativeDropTable[loaded["Token"]];
        {PDSNativeInspectTable[loaded["Token"]][[1]], PDSNativeDropTable[loaded["Token"]][[1]]},
        {"UnknownTable", "UnknownTable"}, TestID -> "dropped-token-invalidated"],
      VerificationTest[setup[]; modes["table.loadTable"] = "TransportFailure";
        PDSNativeLoadTable["SYNTHETIC", "adsl.sas7bdat"];
        {PDSNativeDisconnect[]["ServerCleanup"], $connection, $tables},
        {"Confirmed", None, <||>}, TestID -> "disconnect-clears-unconfirmed-load"],
      VerificationTest[setup[]; loaded = PDSNativeLoadTable["SYNTHETIC", "adsl.sas7bdat"];
        modes["session.endSession"] = "TransportFailure";
        {PDSNativeDisconnect[][[1]], AssociationQ[$connection], Length[$tables]},
        {"TransportFailure", True, 1}, TestID -> "failed-disconnect-retains-records"]
    }
  ];
  TestReport[tests]
];
End[];
Global`SEPIANativeTableTestReport
