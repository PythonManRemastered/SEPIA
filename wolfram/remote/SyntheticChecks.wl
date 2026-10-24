Get[FileNameJoin[{DirectoryName[$InputFileName], "SEPIARemote.wl"}]];
Get[FileNameJoin[{DirectoryName[$InputFileName], "BreastChecks.wl"}]];
Module[{names, data, missing, columns, sample, report, tests},
  missing = <|"ACTDT" -> 95, "TRTSDTM" -> 22, "TRTSDT" -> 2, "TRTEDTM" -> 10,
    "TRTEDT" -> 2, "TRT01AN" -> 2, "EXCLREAN" -> 366, "DSTRDT" -> 1, "DSTRGR1N" -> 1,
    "DSSDDT" -> 1, "DSSDREAN" -> 1, "DEATHDT" -> 121, "DFI" -> 88|>;
  columns = <|
    "USUBJID" -> Table["synthetic-" <> ToString[i], {i, 385}],
    "ARM" -> ConstantArray["Blinded Placebo + Docetaxel", 385],
    "TRT01P" -> ConstantArray["Placebo + Docetaxel", 385],
    "TRT01A" -> Join[ConstantArray["Placebo + Docetaxel", 382], {"", "", "Ramucirumab + Docetaxel"}],
    "SAFFL" -> Join[ConstantArray["Y", 383], {"N", "N"}],
    "ITTFL" -> ConstantArray["Y", 385]|>;
  KeyValueMap[(columns[#1] = Join[ConstantArray[Missing["CASMissing"], #2], ConstantArray[1., 385 - #2]]) &, missing];
  Do[columns["Synthetic" <> ToString[i]] = ConstantArray[0., 385], {i, 110 - Length[columns]}];
  names = Keys[columns]; data = Transpose[Values[columns]];
  sample = <|"ColumnNames" -> names, "Data" -> data|>;
  report = SEPIABreastChecks[sample];
  tests = {
    VerificationTest[FailureQ[SEPIARemote`PDSFetchTable["unknown", "MaxRows" -> 0]], True, TestID -> "invalid-limit"],
    VerificationTest[FailureQ[SEPIARemote`PDSConnect[42]], True, TestID -> "invalid-python-path-type"],
    VerificationTest[report["ArmCountsMatchLocal"], True, TestID -> "synthetic-arm-summary"],
    VerificationTest[report["CheckedMissingCountsMatchLocal"], True, TestID -> "synthetic-missing-summary"],
    VerificationTest[report["DistinctPopulatedIDs"], 385, TestID -> "synthetic-id-summary"],
    VerificationTest[FailureQ[SEPIABreastChecks[<|"Data" -> {}, "ColumnNames" -> {}|>]], True, TestID -> "incomplete-schema"]
  };
  TestReport[tests]
]
