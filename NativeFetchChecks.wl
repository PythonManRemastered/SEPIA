Get[FileNameJoin[{DirectoryName[$InputFileName], "SEPIANative.wl"}]];
Begin["SEPIANative`Private`"];
Global`SEPIANativeFetchTestReport = Module[{setup, table, response, mock, calls, modes, names, schema, rows,
    token, sample, decoded, metadata, tests, cell, pair},
  names = {"ID", "VALUE", "DAY", "ABSENT", "TEXT"};
  schema = MapThread[<|"name" -> #1, "type" -> #2|> &, {names, {"varchar", "double", "date", "double", "varchar"}}];
  rows = {{"b", 1.25, 0, Null, ""}, {"a", 2., 3653, Null, " raw "}};
  metadata = <|"Dimensions" -> {2, 5}, "ColumnNames" -> names|>;
  table[ns_, rs_] := <|"schema" -> (<|"name" -> #|> & /@ ns), "rows" -> rs|>;
  response[result_, severity_: "Normal"] := <|"StatusCode" -> 200, "Cookies" -> {},
    "BodyByteArray" -> ExportByteArray[<|"disposition" -> <|"severity" -> severity|>, "results" -> result|>, "RawJSON"]|>;
  setup[] := ($connection = <|"Session" -> "11111111-2222-3333-4444-555555555555", "Authorization" -> "Basic synthetic", "Affinity" -> ""|>;
    $tables = <|"owned" -> <|"Token" -> "owned", "Caslib" -> "SYNTHETIC", "RelativePath" -> "fixture.sas7bdat",
      "TemporaryTable" -> "sepia_fixture", "TemporaryCaslib" -> "CASUSER", "State" -> "Loaded", "LoadSeverity" -> 0|>|>;
    $busy = False; calls = {}; modes = <||>);
  mock[req_] := Module[{op, mode, rs, sc, cnt, params},
    op = Last[StringSplit[req["URL"], "/"]]; params = parseJSON[req["Body"]];
    AppendTo[calls, <|"Action" -> op, "Parameters" -> params|>];
    mode = Lookup[modes, op, "Normal"];
    If[mode === "TransportFailure", Return[$Failed]];
    If[mode === "Abort", Abort[]];
    If[mode === "Error", Return[response[<||>, "Error"]]];
    Switch[op,
      "table.tableInfo", response[<|"TableInfo" -> table[{"Name", "Rows", "Columns", "Global"},
        {{"SEPIA_FIXTURE", If[Lookup[modes, "Empty", False], 0, 2], 5, 0}}]|>],
      "table.columnInfo", response[<|"ColumnInfo" -> table[{"Column", "ID"}, MapIndexed[{#1, First[#2]} &, names]]|>],
      "table.fetch",
        rs = rows; sc = schema;
        If[mode === "Partial", rs = Take[rs, 1]];
        If[mode === "Ragged", rs[[1]] = Take[rs[[1]], 4]];
        If[mode === "Reordered", sc = Reverse[sc]; rs = Reverse /@ rs];
        If[mode === "WrongColumn", sc[[1]] = <|"name" -> "OTHER", "type" -> "varchar"|>];
        If[mode === "WrongType", sc[[1]] = <|"name" -> "ID", "type" -> "varbinary"|>];
        If[mode === "ArrayCell", rs[[1, 2]] = {1, 2}];
        response[<|"Fetch" -> <|"schema" -> sc, "rows" -> rs|>|>, If[mode === "Warning", "Warning", "Normal"]],
      "table.dropTable" | "session.endSession", response[<||>],
      _, response[<||>, "Error"]
    ]
  ];
  cell[v_, type_, format_: ""] := Catch[decodeCell[v, <|"name" -> "SYNTHETIC", "type" -> type, "format" -> format|>], "SEPIANativeCell"];
  tests = Block[{$connection, $tables, $busy, transport},
    transport[req_, timeout_] := mock[req];
    {
      VerificationTest[setup[]; sample = PDSNativeFetchTable["owned"];
        {sample["Dimensions"], sample["ColumnNames"], sample["Data"][[1]], sample["Severity"]},
        {{2, 5}, names, {"b", 1.25, <|"SEPIAType" -> "Date", "ISO8601" -> "1960-01-01"|>, Missing["CASMissing"], ""}, 0}, TestID -> "native-complete-fetch"],
      VerificationTest[setup[]; PDSNativeFetchTable["owned"];
        Last[calls]["Parameters"], <|"table" -> <|"name" -> "sepia_fixture", "caslib" -> "CASUSER"|>,
          "from" -> 1, "to" -> 2, "maxRows" -> 2, "index" -> False, "format" -> False, "sasTypes" -> False|>, TestID -> "bounded-fetch-parameters"],
      VerificationTest[setup[]; {PDSNativeFetchTable["owned", "MaxRows" -> 1][[1]], Lookup[calls, "Action"]},
        {"FetchLimit", {"table.tableInfo", "table.columnInfo"}}, TestID -> "row-limit-before-record-fetch"],
      VerificationTest[setup[]; {PDSNativeFetchTable["owned", "MaxCells" -> 9][[1]], Lookup[calls, "Action"]},
        {"FetchLimit", {"table.tableInfo", "table.columnInfo"}}, TestID -> "cell-limit-before-record-fetch"],
      VerificationTest[setup[]; {PDSNativeFetchTable["owned", "MaxRows" -> 0][[1]], calls}, {"InvalidLimit", {}}, TestID -> "invalid-fetch-limit"],
      VerificationTest[setup[]; PDSNativeFetchTable["owned", "MaxRows" -> 2, "MaxCells" -> 10]["Dimensions"], {2, 5}, TestID -> "exact-limit-boundary"],
      VerificationTest[setup[]; modes["table.fetch"] = "Partial"; PDSNativeFetchTable["owned"][[1]], "IncompleteFetch", TestID -> "partial-fetch-rejected"],
      VerificationTest[setup[]; modes["table.fetch"] = "Ragged"; PDSNativeFetchTable["owned"][[1]], "IncompleteFetch", TestID -> "ragged-fetch-rejected"],
      VerificationTest[setup[]; modes["table.fetch"] = "Reordered"; PDSNativeFetchTable["owned"]["Data"][[1, 1]], "b", TestID -> "columns-aligned-to-source-schema"],
      VerificationTest[setup[]; modes["table.fetch"] = "WrongColumn"; PDSNativeFetchTable["owned"][[1]], "ColumnMismatch", TestID -> "wrong-fetch-column-rejected"],
      VerificationTest[setup[]; modes["table.fetch"] = "WrongType"; PDSNativeFetchTable["owned"][[1]], "UnsupportedCell", TestID -> "unsupported-wire-type-rejected"],
      VerificationTest[setup[]; modes["table.fetch"] = "ArrayCell"; PDSNativeFetchTable["owned"][[1]], "UnsupportedCell", TestID -> "array-cell-rejected"],
      VerificationTest[setup[]; modes["table.fetch"] = "Warning"; PDSNativeFetchTable["owned"]["Severity"], 1, TestID -> "fetch-warning-preserved"],
      VerificationTest[setup[]; modes["table.fetch"] = "TransportFailure"; sample = PDSNativeFetchTable["owned"];
        {sample[[1]], PDSNativeDropTable["owned"]["Status"]}, {"TransportFailure", "Dropped"}, TestID -> "failed-fetch-retains-cleanup-token"],
      VerificationTest[setup[]; modes["table.fetch"] = "Abort"; CheckAbort[PDSNativeFetchTable["owned"], Null];
        {$busy, $tables["owned"]["State"], PDSNativeDropTable["owned"]["Status"]}, {False, "Loaded", "Dropped"}, TestID -> "aborted-fetch-retains-cleanup-token"],
      VerificationTest[setup[]; modes["Empty"] = True; sample = PDSNativeFetchTable["owned"];
        {sample["Dimensions"], sample["Data"], Lookup[calls, "Action"]}, {{0, 5}, {}, {"table.tableInfo", "table.columnInfo"}}, TestID -> "empty-table-no-record-request"],
      VerificationTest[setup[]; {PDSNativeFetchTable["foreign"][[1]], calls}, {"UnknownTable", {}}, TestID -> "foreign-fetch-token-rejected"],
      VerificationTest[{cell[Null, "double"], cell[-2147483648, "int32"], cell[-9223372036854775808, "int64"]},
        ConstantArray[Missing["CASMissing"], 3], TestID -> "missing-numeric-values"],
      VerificationTest[{cell[-2147483648, "date"], cell[-9223372036854775808, "datetime"], cell[-9223372036854775808, "time"]},
        ConstantArray[Missing["CASMissing"], 3], TestID -> "missing-temporal-values"],
      VerificationTest[{cell["", "char"], cell["  text  ", "varchar"], cell["≥", "string"]},
        {"", "  text  ", "≥"}, TestID -> "strings-preserve-blanks-whitespace-and-unicode"],
      VerificationTest[cell[3653, "date"], <|"SEPIAType" -> "Date", "ISO8601" -> "1970-01-01"|>, TestID -> "cas-date-days"],
      VerificationTest[cell[-1, "date"], <|"SEPIAType" -> "Date", "ISO8601" -> "1959-12-31"|>, TestID -> "pre-epoch-date"],
      VerificationTest[cell[3653., "double", "DATE9."], cell[3653, "date"], TestID -> "sas-date-format"],
      VerificationTest[cell[315662400000000, "datetime"], <|"SEPIAType" -> "DateTime", "ISO8601" -> "1970-01-01T12:00:00"|>, TestID -> "cas-datetime-microseconds"],
      VerificationTest[cell[315662400., "double", "DATETIME20."], cell[315662400000000, "datetime"], TestID -> "sas-datetime-seconds"],
      VerificationTest[cell[-1, "datetime"], <|"SEPIAType" -> "DateTime", "ISO8601" -> "1959-12-31T23:59:59.999999"|>, TestID -> "pre-epoch-subsecond-datetime"],
      VerificationTest[cell[43200000001, "time"], <|"SEPIAType" -> "Time", "ISO8601" -> "12:00:00.000001"|>, TestID -> "cas-time-microseconds"],
      VerificationTest[cell[43200., "double", "TIME8."], <|"SEPIAType" -> "Time", "ISO8601" -> "12:00:00"|>, TestID -> "sas-time-seconds"],
      VerificationTest[FailureQ[cell[.5, "double", "DATE9."]], True, TestID -> "fractional-date-not-silently-truncated"],
      VerificationTest[FailureQ[cell[{1, 2}, "double"]], True, TestID -> "nonscalar-not-stringified"],
      VerificationTest[cell["2020-01-01", "char"], "2020-01-01", TestID -> "date-looking-string-not-recoded"],
      VerificationTest[setup[]; sample = PDSNativeFetchTable["owned"];
        pair = Join[sample, <|"ColumnNames" -> Reverse[names], "Data" -> (Reverse /@ Reverse[sample["Data"]])|>];
        PDSNativeCompareTables[sample, pair, "ID"]["AllValuesMatch"], True, TestID -> "comparison-aligns-by-key-and-column"],
      VerificationTest[setup[]; sample = PDSNativeFetchTable["owned"]; pair = sample; pair["Data"] = ReplacePart[pair["Data"], {1, 2} -> 99.];
        PDSNativeCompareTables[sample, pair, "ID"]["MismatchedCellsByColumn"], <|"VALUE" -> 1|>, TestID -> "comparison-counts-mismatches-without-records"],
      VerificationTest[setup[]; sample = PDSNativeFetchTable["owned"]; pair = sample; pair["Data"] = ReplacePart[pair["Data"], {1, 1} -> "a"];
        PDSNativeCompareTables[sample, pair, "ID"][[1]], "InvalidComparisonKey", TestID -> "duplicate-comparison-key-rejected"],
      VerificationTest[sameCell[1, 1.], True, TestID -> "comparison-int-real-equivalence"]
    }
  ];
  TestReport[tests]
];
End[];
Global`SEPIANativeFetchTestReport
