Module[{dir = CreateDirectory[], path, c, info, r, checks, hash, names, data, pos, bad},
  path = FileNameJoin[{dir, "synthetic.sas7bdat"}];
  Export[path, <|"Subject_ID" -> {"001", "002", "003"},
    "value" -> {1., Missing["NotAvailable"], 3.},
    "tagged" -> {Missing["Tagged", "A"], 2., Missing["Tagged", "Z"]}|>, "SAS7BDAT"];
  c = SEPIAImport`RegisterLocalContribution["SYNTHETIC", "https://example.org", dir];
  hash = FileHash[path, "SHA256"];
  info = SEPIAImport`InspectFile[c, "synthetic.sas7bdat"];
  r = SEPIAImport`ImportContributionFile[c, "synthetic.sas7bdat", None];
  If[FailureQ[r], Return[r]];
  names = r["ColumnNames"]; data = r["Data"];
  pos = Import[path, {"SAS7BDAT", "MissingPositions"}];
  bad = Join[pos, <|"value" -> {1 -> Missing["NotAvailable"]}|>];
  Export[FileNameJoin[{dir, "broken.sas7bdat"}], "Not SAS", "Text"];
  checks = <|
    "SASInspection" -> (info["SupportedImport"] === True && info["FileMetadata"]["Dimensions"] === {3, 3}),
    "NoHeaderRowRemoved" -> (r["Diagnostics"]["DataRowCount"] === 3),
    "NamesPreserved" -> (names === {"Subject_ID", "value", "tagged"}),
    "LeadingZeroID" -> (data[[1, 1]] === "001"),
    "OrdinaryMissing" -> (data[[2, 2]] === Missing["NotAvailable"]),
    "TaggedMissingA" -> (data[[1, 3]] === Missing["Tagged", "A"]),
    "TaggedMissingZ" -> (data[[3, 3]] === Missing["Tagged", "Z"]),
    "AuditPassed" -> TrueQ[r["Diagnostics"]["MissingValueAudit"]["AllPositionsAndMarkersAgree"]],
    "ColumnMetadataRetained" -> (Length[r["FileMetadata"]["ColumnDescriptions"]] === 3),
    "PreviewDisabled" -> MissingQ[r["Preview"]],
    "SourceUnchanged" -> (FileHash[path, "SHA256"] === hash && r["Provenance"]["SHA256"] === IntegerString[hash, 16, 64]),
    "HeaderOptionRejected" -> MatchQ[SEPIAImport`ImportContributionFile[c, "synthetic.sas7bdat", None, "FirstRowIsHeader" -> True], Failure["InvalidHeaderOption", _]],
    "SheetStringRejected" -> MatchQ[SEPIAImport`ImportContributionFile[c, "synthetic.sas7bdat", "Sheet1"], Failure["InvalidSelection", _]],
    "SizeCapRejected" -> MatchQ[SEPIAImport`ImportContributionFile[c, "synthetic.sas7bdat", None, "MaxFileBytes" -> 1], Failure["FileSizeLimit", _]],
    "CorruptSASRejected" -> FailureQ[SEPIAImport`InspectFile[c, "broken.sas7bdat"]],
    "WrongMissingPositionRejected" -> FailureQ[SEPIAImport`Private`sasAudit[data, names, bad]],
    "WrongMissingMarkerRejected" -> FailureQ[SEPIAImport`Private`sasAudit[data, names, Join[pos, <|"value" -> {2 -> Missing["Tagged", "A"]}|>]]],
    "UnknownMetadataColumnRejected" -> FailureQ[SEPIAImport`Private`sasAudit[data, names, Join[pos, <|"unknown" -> {}|>]]],
    "BarePositionListRejected" -> FailureQ[SEPIAImport`Private`sasAudit[data, names, Join[pos, <|"value" -> {2}|>]]]
  |>;
  <|"WolframVersion" -> $Version, "Passed" -> Count[Values[checks], True],
    "Total" -> Length[checks], "Checks" -> checks, "SyntheticDirectory" -> dir|>
]
