Get[FileNameJoin[{DirectoryName[$InputFileName], "SEPIANative.wl"}]];
Begin["SEPIANative`Private`"];
Global`SEPIANativeSizingTestReport = Module[{setup, prompts, requests, consent, dimensions, tests, old},
  setup[dims_] := (dimensions = dims; prompts = {}; requests = {}; consent = True;
    $connection = "mock"; $tables = <||>; uiNewSession[]; PDSNativeClearImportedData[];
    $uiTable = <|"Token" -> "owned", "RelativePath" -> "synthetic.sas7bdat", "Caslib" -> "SYNTHETIC"|>;
    $uiMetadata = <|"Dimensions" -> dims|>;
  );
  tests = Block[{$connection, $tables, $uiNotebook = None, $uiBusy, $uiBlocked, $uiListing, $uiSelected,
      $uiTable, $uiMetadata, $uiImported, $uiImportSummary, $uiTimings, $uiStatus, $uiTone,
      PDSNativeFetchTable, uiConfirmLargeImport},
    uiConfirmLargeImport[dims_, path_] := (AppendTo[prompts, {dims, path}]; consent);
    PDSNativeFetchTable[token_String, rules___Rule] := (
      AppendTo[requests, <|"Token" -> token, "Options" -> Association[rules]|>];
      <|"Dimensions" -> dimensions, "Severity" -> 0, "ElapsedSeconds" -> .1,
        "Provenance" -> <|"RelativePath" -> "synthetic.sas7bdat", "Caslib" -> "SYNTHETIC"|>|>
    );
    {
      VerificationTest[setup[{2083, 4}]; uiImport[];
        {prompts, First[requests]["Options"], $uiBusy},
        {{}, <|"MaxRows" -> 2083, "MaxCells" -> 8332|>, False}, TestID -> "statin-import-sizes-automatically"],
      VerificationTest[setup[{7355, 115}]; uiImport[];
        {Length[prompts], First[requests]["Options"]},
        {0, <|"MaxRows" -> 7355, "MaxCells" -> 845825|>}, TestID -> "adae-under-confirmation-threshold"],
      VerificationTest[setup[{1000, 1000}]; uiImport[];
        {Length[prompts], Length[requests]}, {0, 1}, TestID -> "exactly-one-million-no-confirmation"],
      VerificationTest[setup[{1000001, 1}]; uiImport[];
        {prompts, First[requests]["Options"]},
        {{{{1000001, 1}, "synthetic.sas7bdat"}}, <|"MaxRows" -> 1000001, "MaxCells" -> 1000001|>},
        TestID -> "above-one-million-asks-before-full-fetch"],
      VerificationTest[setup[{25871, 81}]; consent = False; uiImport[];
        {Length[prompts], requests, $uiBusy, $uiTable["Token"], $uiTone},
        {1, {}, False, "owned", "Info"}, TestID -> "cancel-fetches-no-records-and-keeps-table"],
      VerificationTest[setup[{25871, 81}]; $uiImported = <|"Previous" -> True|>;
        consent = False; uiImport[]; PDSNativeImportedData[], <|"Previous" -> True|>,
        TestID -> "cancel-preserves-previous-local-import"],
      VerificationTest[setup[{25871, 81}]; consent = $Canceled; uiImport[];
        {requests, $uiBusy}, {{}, False}, TestID -> "closing-confirmation-cancels"],
      VerificationTest[setup[{0, 4}]; uiImport[]; First[requests]["Options"],
        <|"MaxRows" -> 1, "MaxCells" -> 1|>, TestID -> "empty-table-uses-valid-fetch-bounds"],
      VerificationTest[setup[{2, "invalid"}]; uiImport[];
        {requests, $uiTone}, {{}, "Error"}, TestID -> "invalid-dimensions-block-import"],
      VerificationTest[setup[{385, 110}]; FreeQ[uiContents[], HoldPattern[InputField[_, Number, ___]]],
        True, TestID -> "manual-numeric-limit-fields-removed"]
    }
  ];
  TestReport[tests]
];
End[];
Global`SEPIANativeSizingTestReport
