Module[{dir = CreateDirectory[], r, c, selected, sheet, imported, stale, checks},
  Export[FileNameJoin[{dir, "fixture.xlsx"}], {{"ID"}, {"001"}}, "XLSX"];
  r = SEPIAImport`RegisterLocalContribution["SYNTHETIC", "https://example.org", dir];
  c = SEPIACatalog`CreateContributionCatalog[{r}];
  selected = SEPIACatalog`LookupContribution[c, "SYNTHETIC"];
  sheet = First[SEPIAImport`InspectFile[selected, "fixture.xlsx"]["Sheets"]];
  imported = SEPIAImport`ImportContributionFile[selected, "fixture.xlsx", sheet, "FirstRowIsHeader" -> True];
  stale = Join[r, <|"RootDirectory" -> FileNameJoin[{dir, "absent"}]|>];
  checks = <|
    "ExactRecordPreserved" -> (selected === r),
    "UnknownDistinctFailure" -> MatchQ[SEPIACatalog`LookupContribution[c, "unknown"], Failure["UnregisteredContribution", _]],
    "NoCaseFolding" -> FailureQ[SEPIACatalog`LookupContribution[c, "synthetic"]],
    "DuplicateRejected" -> MatchQ[SEPIACatalog`CreateContributionCatalog[{r, r}], Failure["DuplicateContributionID", _]],
    "FailedRegistrationRejected" -> FailureQ[SEPIACatalog`CreateContributionCatalog[{Failure["x", <||>]}]],
    "MalformedCatalogRejected" -> FailureQ[SEPIACatalog`LookupContribution[<||>, "x"]],
    "EmptyCatalogValid" -> AssociationQ[SEPIACatalog`CreateContributionCatalog[{}]],
    "InventoryIntegration" -> (Length[SEPIAImport`ListContributionFiles[selected]] === 1),
    "ImportIntegration" -> (AssociationQ[imported] && imported["Data"][[2, 1]] === "001"),
    "MissingFolderRemainsRegistered" -> AssociationQ[SEPIACatalog`LookupContribution[SEPIACatalog`CreateContributionCatalog[{stale}], "SYNTHETIC"]],
    "MissingFolderInventoryFailure" -> MatchQ[SEPIAImport`ListContributionFiles[stale], Failure["FolderNotFound", _]]
  |>;
  <|"WolframVersion" -> $Version, "Passed" -> Count[Values[checks], True],
    "Total" -> Length[checks], "Checks" -> checks|>
]
