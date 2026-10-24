Module[{dir = CreateDirectory[], c, path, info, sheet, r, checks, hash},
  path = FileNameJoin[{dir, "fixture.xlsx"}];
  Export[path, {{"Subject_ID", "value"}, {"001", 3}, {"002", 7}}, "XLSX"];
  Export[FileNameJoin[{dir, "notes.txt"}], "Synthetic supporting document", "Text"];
  Export[FileNameJoin[{dir, "broken.xlsx"}], "Not a workbook", "Text"];
  CreateDirectory[FileNameJoin[{dir, "nested"}]];
  CopyFile[path, FileNameJoin[{dir, "nested", "fixture.xlsx"}]];
  c = SEPIAImport`RegisterLocalContribution["SYNTHETIC", "https://example.org", dir];
  info = SEPIAImport`InspectFile[c, "fixture.xlsx"];
  sheet = First[info["Sheets"]]; hash = FileHash[path, "SHA256"];
  r = SEPIAImport`ImportContributionFile[c, "fixture.xlsx", sheet, "FirstRowIsHeader" -> True];
  checks = <|
    "InventoryIncludesNestedDuplicates" -> (Length[SEPIAImport`ListContributionFiles[c]] === 4),
    "ImportDimensions" -> (r["Diagnostics"]["DataRowCount"] === 2 && r["Diagnostics"]["ColumnCount"] === 2),
    "StringIDPreserved" -> (r["Data"][[2, 1]] === "001"),
    "PreviewDisabled" -> MissingQ[r["Preview"]],
    "ChecksumMatchesUnchangedSource" -> (r["Provenance"]["SHA256"] === IntegerString[hash, 16, 64] && FileHash[path, "SHA256"] === hash),
    "WrongSheetRejected" -> MatchQ[SEPIAImport`ImportContributionFile[c, "fixture.xlsx", "absent"], Failure["SheetNotFound", _]],
    "MissingFileRejected" -> FailureQ[SEPIAImport`InspectFile[c, "missing.xlsx"]],
    "TraversalRejected" -> FailureQ[SEPIAImport`InspectFile[c, "../fixture.xlsx"]],
    "UnsupportedImportRejected" -> MatchQ[SEPIAImport`ImportContributionFile[c, "notes.txt", "none"], Failure["UnsupportedFormat", _]],
    "CorruptWorkbookRejected" -> FailureQ[SEPIAImport`InspectFile[c, "broken.xlsx"]],
    "SizeLimitRejected" -> MatchQ[SEPIAImport`ImportContributionFile[c, "fixture.xlsx", sheet, "MaxFileBytes" -> 1], Failure["FileSizeLimit", _]],
    "InvalidOptionsRejected" -> MatchQ[SEPIAImport`ImportContributionFile[c, "fixture.xlsx", sheet, "PreviewRows" -> -1], Failure["InvalidOptions", _]],
    "MissingFolderRejected" -> FailureQ[SEPIAImport`RegisterLocalContribution["S", "https://example.org", FileNameJoin[{dir, "absent"}]]]
  |>;
  <|"WolframVersion" -> $Version, "Passed" -> Count[Values[checks], True],
    "Total" -> Length[checks], "Checks" -> checks, "SyntheticDirectory" -> dir|>
]
