Clear[SEPIABreastChecks];
SEPIABreastChecks[result_?FailureQ] := result;
SEPIABreastChecks[result_Association] := Module[
  {names, data, required, col, ids, good, arms, expectedArms, expectedMissing,
   missingCounts, sameCounts, counts},
  names = Lookup[result, "ColumnNames", {}]; data = Lookup[result, "Data", {}];
  required = {"USUBJID", "ARM", "TRT01P", "TRT01A", "SAFFL", "ITTFL", "ACTDT", "TRTSDTM",
    "TRTSDT", "TRTEDTM", "TRTEDT", "TRT01AN", "EXCLREAN", "DSTRDT", "DSTRGR1N", "DSSDDT",
    "DSSDREAN", "DEATHDT", "DFI"};
  If[Dimensions[data] =!= {385, 110} || Length[names] =!= 110 ||
     Length[DeleteDuplicates[names]] =!= 110 || !ContainsAll[names, required],
    Return[Failure["UnexpectedSchema", <|"MessageTemplate" -> "Expected complete breast ADSL with 385 rows, 110 unique columns and all checked fields."|>]]];
  col[name_] := data[[All, First[FirstPosition[names, name]]]];
  ids = col["USUBJID"];
  good = Select[ids, !MissingQ[#] && !(StringQ[#] && StringTrim[#] === "") &];
  expectedArms = <|
    "ARM" -> <|"Blinded Placebo + Docetaxel" -> 385|>,
    "TRT01P" -> <|"Placebo + Docetaxel" -> 385|>,
    "TRT01A" -> <|"Placebo + Docetaxel" -> 382, "" -> 2, "Ramucirumab + Docetaxel" -> 1|>,
    "SAFFL" -> <|"Y" -> 383, "N" -> 2|>, "ITTFL" -> <|"Y" -> 385|>|>;

  counts[values_] := Counts[Select[values, StringQ]];
  arms = AssociationMap[counts[col[#]] &, Keys[expectedArms]];
  sameCounts[a_, b_] := KeySort[a] === KeySort[b];
  expectedMissing = <|"ACTDT" -> 95, "TRTSDTM" -> 22, "TRTSDT" -> 2, "TRTEDTM" -> 10,
    "TRTEDT" -> 2, "TRT01AN" -> 2, "EXCLREAN" -> 366, "DSTRDT" -> 1, "DSTRGR1N" -> 1,
    "DSSDDT" -> 1, "DSSDREAN" -> 1, "DEATHDT" -> 121, "DFI" -> 88|>;
  missingCounts = AssociationMap[Count[col[#], _Missing] &, Keys[expectedMissing]];
  <|"Rows" -> Length[data], "BlankOrMissingIDs" -> Length[ids] - Length[good],
    "DistinctPopulatedIDs" -> Length[DeleteDuplicates[good]],
    "RepeatedIDOccurrencesBeyondFirst" -> Length[good] - Length[DeleteDuplicates[good]],
    "AllPopulatedIDsAreStrings" -> AllTrue[good, StringQ],
    "ArmAndPopulationCounts" -> arms,
    "ArmCountsMatchLocal" -> AllTrue[Keys[expectedArms], sameCounts[arms[#], expectedArms[#]] &],
    "MissingCountsFor13CheckedColumns" -> missingCounts,
    "CheckedMissingCountsMatchLocal" -> sameCounts[missingCounts, expectedMissing]|>
];
