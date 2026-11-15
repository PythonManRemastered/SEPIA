Get[FileNameJoin[{DirectoryName[$InputFileName], "SEPIANative.wl"}]];
Begin["SEPIANative`Private`"];
Global`SEPIANativeNavigationTestReport = Module[{setup, response, fileInfo, calls, entries, modes, mock,
    listing, child, loaded, result, tests, names, original},
  fileInfo[rows_] := <|"FileInfo" -> <|"schema" -> (<|"name" -> #|> & /@ {"Name", "Size", "Permission"}), "rows" -> rows|>|>;
  response[result_, severity_: "Normal"] := <|"StatusCode" -> 200, "Cookies" -> {},
    "BodyByteArray" -> ExportByteArray[<|"disposition" -> <|"severity" -> severity|>, "results" -> result|>, "RawJSON"]|>;
  setup[] := ($connection = <|"Session" -> "11111111-2222-3333-4444-555555555555", "Authorization" -> "Basic synthetic", "Affinity" -> ""|>;
    $tables = <||>; $listings = <||>; $busy = False; calls = {}; modes = <||>;
    entries = {{"dataFiles_1003", 4096, "drwxr-xr-x"}, {"guide.pdf", 100, "-rw-r--r--"}, {"archive.zip", 100, "-rw-r--r--"}};
    uiNewSession[]; PDSNativeClearImportedData[]; $uiUID = "SYNTHETIC";
  );
  mock[req_] := Module[{op, parameters, mode, rows, cols, tableName},
    op = Last[StringSplit[req["URL"], "/"]]; parameters = parseJSON[req["Body"]];
    AppendTo[calls, <|"Action" -> op, "Parameters" -> parameters|>]; mode = Lookup[modes, op, "Normal"];
    If[mode === "TransportFailure", Return[$Failed]];
    Switch[op,
      "table.fileInfo", rows = If[KeyExistsQ[parameters, "path"], {{"adsl.sas7bdat", 100, "-rw-r--r--"}}, entries];
        If[KeyExistsQ[modes, "Entries"], rows = modes["Entries"]]; response[fileInfo[rows]],
      "table.loadTable" | "table.dropTable" | "session.endSession", response[<||>],
      "table.tableInfo", tableName = First[Values[$tables]]["TemporaryTable"];
        response[<|"TableInfo" -> <|"schema" -> (<|"name" -> #|> & /@ {"Name", "Rows", "Columns", "Global"}), "rows" -> {{tableName, 2, 2, 0}}|>|>],
      "table.columnInfo", response[<|"ColumnInfo" -> <|"schema" -> (<|"name" -> #|> & /@ {"Column", "ID"}), "rows" -> {{"ID", 1}, {"VALUE", 2}}|>|>],
      "table.fetch", response[<|"Fetch" -> <|"schema" -> {<|"name" -> "ID", "type" -> "varchar"|>, <|"name" -> "VALUE", "type" -> "double"|>},
        "rows" -> {{"synthetic-a", 1.}, {"synthetic-b", 2.}}|>|>],
      _, response[<||>, "Error"]
    ]
  ];
  tests = Block[{$connection, $tables, $listings, $busy, transport, $uiNotebook = None, $uiBusy, $uiBlocked,
      $uiUID, $uiListing, $uiSelected, $uiTable, $uiMetadata, $uiImported, $uiImportSummary, $uiTimings, $uiStatus, $uiTone},
    transport[req_, timeout_] := mock[req];
    {
      VerificationTest[setup[]; listing = PDSNativeBrowseContribution["SYNTHETIC"];
        {Lookup[listing["Entries"], "Kind"], Lookup[listing["Entries"], "SupportedImport"], listing["ParentDirectory"]},
        {{"Directory", "File", "File"}, {False, False, False}, ""}, TestID -> "native-root-listing"],
      VerificationTest[setup[]; listing = PDSNativeBrowseContribution["SYNTHETIC"];
        child = PDSNativeOpenEntry[listing, 1];
        {child["Directory"], child["ParentDirectory"], child["Entries"][[1]]["RelativePath"]},
        {"dataFiles_1003", "", "dataFiles_1003/adsl.sas7bdat"}, TestID -> "native-folder-open"],
      VerificationTest[setup[]; listing = PDSNativeBrowseContribution["SYNTHETIC", "a/b"];
        {listing["ParentDirectory"], listing["Entries"][[1]]["RelativePath"]}, {"a", "a/b/adsl.sas7bdat"}, TestID -> "nested-parent-path"],
      VerificationTest[setup[]; modes["Entries"] = {{"a/b/adsl.sas7bdat", 100, "-rw-r--r--"}};
        PDSNativeBrowseContribution["SYNTHETIC", "a/b"]["Entries"][[1]]["RelativePath"], "a/b/adsl.sas7bdat", TestID -> "root-relative-entry-path"],
      VerificationTest[setup[]; modes["Entries"] = {{"other/adsl.sas7bdat", 100, "-rw-r--r--"}};
        PDSNativeBrowseContribution["SYNTHETIC", "a"][[1]], "AmbiguousListingPath", TestID -> "ambiguous-path-rejected"],
      VerificationTest[setup[]; modes["Entries"] = {{"../adsl.sas7bdat", 100, "-rw-r--r--"}};
        PDSNativeBrowseContribution["SYNTHETIC"][[1]], "InvalidListingPath", TestID -> "unsafe-entry-path-rejected"],
      VerificationTest[setup[]; modes["Entries"] = {{".", 0, "drwxr-xr-x"}, {"..", 0, "drwxr-xr-x"}};
        PDSNativeBrowseContribution["SYNTHETIC"]["Entries"], {}, TestID -> "dot-entries-excluded"],
      VerificationTest[setup[]; listing = PDSNativeBrowseContribution["SYNTHETIC"];
        {PDSNativeOpenEntry[listing, 2][[1]], PDSNativeOpenEntry[listing, 3][[1]], Lookup[calls, "Action"]},
        {"UnsupportedEntry", "UnsupportedEntry", {"table.fileInfo"}}, TestID -> "documents-and-archives-not-loaded"],
      VerificationTest[setup[]; PDSNativeOpenEntry[<|"ListingToken" -> "stale"|>, 1][[1]], "UnknownListing", TestID -> "stale-listing-rejected"],
      VerificationTest[setup[]; listing = PDSNativeBrowseContribution["SYNTHETIC"];
        {PDSNativeOpenEntry[listing, 0][[1]], PDSNativeOpenEntry[listing, 99][[1]], PDSNativeOpenEntry[listing, True][[1]]},
        {"InvalidSelection", "InvalidSelection", "InvalidSelection"}, TestID -> "invalid-selection-rejected"],
      VerificationTest[setup[]; listing = PDSNativeBrowseContribution["SYNTHETIC", "dataFiles_1003"];
        listing["Entries"] = {<|"RelativePath" -> "tampered.sas7bdat"|>};
        loaded = PDSNativeOpenEntry[listing, 1]; loaded["RelativePath"], "dataFiles_1003/adsl.sas7bdat", TestID -> "selection-uses-owned-listing"],
      VerificationTest[setup[]; listing = PDSNativeBrowseContribution["SYNTHETIC", "dataFiles_1003"];
        loaded = PDSNativeOpenEntry[listing, 1];
        {loaded["ContributionID"], $tables[loaded["Token"]]["ContributionID"], Lookup[calls, "Action"]},
        {"SYNTHETIC", "SYNTHETIC", {"table.fileInfo", "table.loadTable"}}, TestID -> "selected-table-identity-and-no-fetch"],
      VerificationTest[setup[]; listing = PDSNativeBrowseContribution["SYNTHETIC"];
        PDSNativeDisconnect[]; {$listings, PDSNativeOpenEntry[listing, 1][[1]]}, {<||>, "NotConnected"}, TestID -> "disconnect-invalidates-listings"],
      VerificationTest[setup[]; uiBrowseRoot[]; $uiSelected = 1; uiNavigate["Open"]; $uiSelected = 1;
        uiLoad[]; uiImport[];
        {PDSNativeImportedData[]["Dimensions"], $uiMetadata["Dimensions"], $uiImportSummary["Source"], $uiTone},
        {{2, 2}, {2, 2}, "dataFiles_1003/adsl.sas7bdat", "Success"}, TestID -> "native-ui-end-to-end-adapter"],
      VerificationTest[setup[]; uiBrowseRoot[]; $uiSelected = 1; uiNavigate["Open"]; $uiSelected = 1;
        uiLoad[]; uiImport[]; uiNavigate["Up"];
        {$uiListing["Directory"], $uiTable, FailureQ[PDSNativeImportedData[]], Count[Lookup[calls, "Action"], "table.dropTable"]},
        {"", None, True, 1}, TestID -> "up-releases-table-and-clears-import"],
      VerificationTest[setup[]; uiBrowseRoot[]; $uiSelected = 1; uiNavigate["Open"]; $uiSelected = 1;
        uiLoad[]; uiImport[]; uiSetLibrary["OTHER"];
        {$uiListing, $uiSelected, $uiMetadata, FailureQ[PDSNativeImportedData[]], AssociationQ[$uiTable]},
        {None, 0, None, True, True}, TestID -> "editing-library-invalidates-selection-but-retains-cleanup-token"],
      VerificationTest[setup[]; uiBrowseRoot[]; $uiSelected = 1; uiNavigate["Open"]; $uiSelected = 1;
        uiLoad[]; modes["table.dropTable"] = "TransportFailure"; original = Length[calls]; uiBrowseRoot[];
        {$uiBusy, $uiBlocked, Length[calls] - original, AssociationQ[$uiTable]}, {False, True, 1, True}, TestID -> "uncertain-cleanup-blocks-ui-replacement"],
      VerificationTest[setup[]; uiBrowseRoot[]; $uiSelected = 1; uiNavigate["Open"]; $uiSelected = 1;
        modes["table.loadTable"] = "TransportFailure"; uiLoad[];
        {$uiBusy, $uiBlocked, Length[$tables], uiReady[]}, {False, True, 1, False}, TestID -> "uncertain-load-blocks-ui"],
      VerificationTest[setup[]; uiBrowseRoot[]; $uiSelected = 1; uiNavigate["Open"]; $uiSelected = 1;
        uiLoad[]; uiImport[]; PDSNativeDisconnect[];
        {$uiTable, $uiListing, $tables, AssociationQ[PDSNativeImportedData[]]},
        {None, None, <||>, True}, TestID -> "disconnect-preserves-local-native-import"]
    }
  ];
  TestReport[tests]
];
End[];
Global`SEPIANativeNavigationTestReport
