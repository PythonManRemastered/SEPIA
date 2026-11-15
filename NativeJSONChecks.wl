Get[FileNameJoin[{DirectoryName[$InputFileName], "SEPIANative.wl"}]];
Begin["SEPIANative`Private`"];
Global`SEPIANativeJSONTestReport = Module[{payload, text, tests},
  payload = <|"results" -> <|"ColumnInfo" -> <|"schema" -> {<|"name" -> "Column"|>, <|"name" -> "Label"|>},
    "rows" -> {{"AGEGR1", "Age ≥ 65 years"}, {"BMI", "kg/m²"}, {"NOTE", "Patient’s status"}}|>|>|>;
  text = "{\"label\":\"Age ≥ 65 years\"}";
  tests = {
    VerificationTest[parseJSON[text], <|"label" -> "Age ≥ 65 years"|>, TestID -> "unicode-metadata-label"],
    VerificationTest[parseJSON["{\"label\":\"kg/m²; ≤; é; α; Patient’s status\"}"]["label"],
      "kg/m²; ≤; é; α; Patient’s status", TestID -> "unicode-label-characters-preserved"],
    VerificationTest[parseJSON["{\"label\":\"\\u2265\"}"], <|"label" -> "≥"|>, TestID -> "existing-unicode-escape"],
    VerificationTest[parseJSON[ExportByteArray[payload, "RawJSON"]], payload, TestID -> "nested-metadata-roundtrip"],
    VerificationTest[parseJSON["{\"label\":\"line one\\nline two\"}"]["label"],
      "line one\nline two", TestID -> "escaped-newline-preserved"],
    VerificationTest[parseJSON["{\"rows\":385,\"cols\":110,\"missing\":null,\"flag\":false}"],
      <|"rows" -> 385, "cols" -> 110, "missing" -> Null, "flag" -> False|>, TestID -> "scalar-types-preserved"],
    VerificationTest[parseJSON["<html></html>"], $Failed, TestID -> "html-still-rejected"],
    VerificationTest[parseJSON["{\"label\":\"unfinished"], $Failed, TestID -> "truncated-json-still-rejected"],
    VerificationTest[parseJSON[None], $Failed, TestID -> "nonstring-body-rejected"],
    VerificationTest[Block[{$connection = <|"Session" -> "11111111-2222-3333-4444-555555555555",
        "Authorization" -> "Basic synthetic", "Affinity" -> ""|>, transport},
      transport[req_, timeout_] := <|"StatusCode" -> 200, "Body" ->
        "{\"disposition\":{\"severity\":\"Normal\"},\"results\":{\"Label\":\"Age ≥ 65 years\"}}", "Cookies" -> {}|>;
      action["table.columnInfo", <||>, 60]["Results"]],
      <|"Label" -> "Age ≥ 65 years"|>, TestID -> "unicode-response-through-action"],
    VerificationTest[Block[{$connection = <|"Session" -> "11111111-2222-3333-4444-555555555555",
        "Authorization" -> "Basic synthetic", "Affinity" -> ""|>, transport},
      transport[req_, timeout_] := <|"StatusCode" -> 200,
        "BodyByteArray" -> ExportByteArray[<|"disposition" -> <|"severity" -> "Normal"|>,
          "results" -> payload|>, "RawJSON"], "Body" -> "Not the byte payload", "Cookies" -> {}|>;
      action["table.columnInfo", <||>, 60]["Results"]],
      payload, TestID -> "raw-http-bytes-preserve-unicode-labels"]
  };
  TestReport[tests]
];
End[];
Global`SEPIANativeJSONTestReport
