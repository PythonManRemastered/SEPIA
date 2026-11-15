Get[FileNameJoin[{DirectoryName[$InputFileName], "SEPIANative.wl"}]];
Begin["SEPIANative`Private`"];
Global`SEPIANativeTestReport = Module[{response, success, fixture, session, requests, queue, setup, tests},
  session = "11111111-2222-3333-4444-555555555555";
  response[status_, payload_] := <|"StatusCode" -> status,
    "Body" -> ExportString[payload, "RawJSON"], "Cookies" -> {}|>;
  success[results_: <||>, severity_: "Normal"] := response[200,
    <|"disposition" -> <|"severity" -> severity|>, "results" -> results|>];
  fixture = <|"FileInfo" -> <|"_ctb" -> True,
    "schema" -> {<|"name" -> "Permission"|>, <|"name" -> "Name"|>, <|"name" -> "Size"|>},
    "rows" -> {{"drwxr-xr-x", "dataFiles_1003", 4096}, {"-rw-r--r--", "adsl.sas7bdat", 2359296}}|>|>;
  setup[] := ($connection = None; $tables = <||>; $busy = False; requests = {}; queue = {});
  tests = Block[{$connection, $tables, $busy, transport, credentialsDialog},
    transport[req_, timeout_] := Module[{answer},
      AppendTo[requests, req];
      If[queue === {}, Return[$Failed]];
      answer = First[queue]; queue = Rest[queue]; answer
    ];
    credentialsDialog[] := {"synthetic-user", "synthetic-password"};
    {
      VerificationTest[setup[]; queue = {response[200, <|"session" -> session|>]};
        PDSNativeConnect[]["Status"], "Connected", TestID -> "native-login"],
      VerificationTest[setup[]; queue = {response[200, <|"session" -> session|>]};
        PDSNativeConnect[]; {requests[[1]]["Method"], requests[[1]]["URL"]},
        {"PUT", $endpoint <> "cas/sessions"}, TestID -> "legacy-create-session-route"],
      VerificationTest[setup[]; queue = {response[200, <|"session" -> session|>]};
        FreeQ[PDSNativeConnect[], "synthetic-password" | "synthetic-user" | session], True,
        TestID -> "login-output-excludes-credentials-and-session"],
      VerificationTest[setup[]; queue = {response[401, <|"error" -> "synthetic-password"|>]};
        {PDSNativeConnect[][[1]], $connection}, {"AuthenticationRejected", None}, TestID -> "rejected-login"],
      VerificationTest[setup[]; queue = {response[302, <||>]}; PDSNativeConnect[][[1]],
        "UnexpectedRedirect", TestID -> "redirect-rejected"],
      VerificationTest[setup[]; queue = {<|"StatusCode" -> 200, "Body" -> "<html></html>"|>};
        PDSNativeConnect[][[1]], "InvalidJSON", TestID -> "html-is-not-login-success"],
      VerificationTest[setup[]; queue = {response[200, <|"session" -> "../invalid"|>]};
        {PDSNativeConnect[][[1]], $connection}, {"InvalidSessionResponse", None}, TestID -> "invalid-session-id"],
      VerificationTest[setup[]; queue = {$Failed};
        {PDSNativeConnect[][[1]], Length[requests]}, {"TransportFailure", 1}, TestID -> "no-automatic-retry"],
      VerificationTest[setup[]; PDSNativeConnect["Timeout" -> 0][[1]], "InvalidTimeout", TestID -> "invalid-timeout"],
      VerificationTest[setup[]; queue = {response[200, <|"session" -> session|>]}; PDSNativeConnect[];
        {PDSNativeConnect[][[1]], Length[requests]}, {"AlreadyConnected", 1}, TestID -> "duplicate-login-blocked"],
      VerificationTest[setup[]; PDSNativeListFiles["SYNTHETIC"][[1]], "NotConnected", TestID -> "listing-requires-session"],
      VerificationTest[setup[]; queue = {response[200, <|"session" -> session|>], success[fixture]};
        PDSNativeConnect[]; PDSNativeListFiles["SYNTHETIC"]["Entries"],
        {<|"Name" -> "dataFiles_1003", "Bytes" -> 4096, "Permissions" -> "drwxr-xr-x"|>,
         <|"Name" -> "adsl.sas7bdat", "Bytes" -> 2359296, "Permissions" -> "-rw-r--r--"|>}, TestID -> "file-info-decoding"],
      VerificationTest[setup[]; queue = {response[200, <|"session" -> session|>], success[fixture]};
        PDSNativeConnect[]; PDSNativeListFiles["SYNTHETIC", "dataFiles_1003"];
        ImportString[requests[[2]]["Body"], "RawJSON"],
        <|"caslib" -> "SYNTHETIC", "allFiles" -> True, "path" -> "dataFiles_1003"|>, TestID -> "nested-listing-parameters"],
      VerificationTest[setup[]; queue = {response[200, <|"session" -> session|>], success[fixture, "Warning"]};
        PDSNativeConnect[]; PDSNativeListFiles["SYNTHETIC"]["Severity"], 1, TestID -> "warning-preserved"],
      VerificationTest[setup[]; queue = {response[200, <|"session" -> session|>], success[fixture, "Error"]};
        PDSNativeConnect[]; PDSNativeListFiles["SYNTHETIC"][[1]], "CASActionFailed", TestID -> "http-200-action-error"],
      VerificationTest[setup[]; queue = {response[200, <|"session" -> session|>], success[fixture, "Unexpected"]};
        PDSNativeConnect[]; PDSNativeListFiles["SYNTHETIC"][[1]], "UnknownSeverity", TestID -> "unknown-severity-rejected"],
      VerificationTest[FailureQ[fileEntries[<|"FileInfo" -> <|"schema" -> {<|"name" -> "Name"|>, <|"name" -> "Name"|>}, "rows" -> {}|>|>]],
        True, TestID -> "duplicate-columns-rejected"],
      VerificationTest[FailureQ[fileEntries[<|"FileInfo" -> <|"schema" -> {<|"name" -> "Name"|>, <|"name" -> "Size"|>}, "rows" -> {{"x"}}|>|>]],
        True, TestID -> "ragged-rows-rejected"],
      VerificationTest[fileEntries[<|"FileInfo" -> <|"schema" -> {<|"name" -> "Name"|>, <|"name" -> "Size"|>}, "rows" -> {}|>|>],
        {}, TestID -> "empty-directory"],
      VerificationTest[FailureQ[fileEntries[<|"FileInfo" -> <|"schema" -> {<|"name" -> "Name"|>, <|"name" -> "Size"|>}, "rows" -> {{"x", -1}}|>|>]],
        True, TestID -> "invalid-size-rejected"],
      VerificationTest[setup[]; queue = {response[200, <|"session" -> session|>]}; PDSNativeConnect[];
        {PDSNativeListFiles["SYNTHETIC", "../x"][[1]], Length[requests]}, {"InvalidPath", 1}, TestID -> "traversal-rejected-before-request"],
      VerificationTest[validPath /@ {"/x", "a\\b", "a/../b", "a/" <> FromCharacterCode[0]},
        {False, False, False, False}, TestID -> "unsafe-paths"],
      VerificationTest[setup[]; queue = {response[200, <|"session" -> session|>], success[]};
        PDSNativeConnect[]; {PDSNativeDisconnect[]["Status"], $connection,
          requests[[2]]["URL"]}, {"Closed", None, $endpoint <> "cas/sessions/" <> session <> "/actions/session.endSession"},
        TestID -> "disconnect-ends-server-session"],
      VerificationTest[setup[]; queue = {response[200, <|"session" -> session|>], $Failed}; PDSNativeConnect[];
        {PDSNativeDisconnect[][[1]], AssociationQ[$connection]}, {"TransportFailure", True}, TestID -> "failed-close-retains-cleanup-state"],
      VerificationTest[setup[]; {PDSNativeDisconnect[]["Status"], requests}, {"AlreadyClosed", {}}, TestID -> "disconnect-idempotent"],
      VerificationTest[setup[]; $busy = True; {PDSNativeConnect[][[1]], requests}, {"Busy", {}}, TestID -> "busy-guard"],
      VerificationTest[setup[]; CheckAbort[run[Abort[]], Null]; $busy, False, TestID -> "abort-releases-busy-guard"],
      VerificationTest[setup[]; queue = {Append[response[200, <|"session" -> session|>],
          "Cookies" -> {<|"Name" -> "tkhttp-id", "Content" -> "synthetic-affinity"|>}], success[fixture]};
        PDSNativeConnect[]; PDSNativeListFiles["SYNTHETIC"];
        Lookup[Association[requests[[2]]["Headers"]], "tkhttp-id"], "synthetic-affinity", TestID -> "affinity-header-preserved"]
    }
  ];
  TestReport[tests]
];
End[];
Global`SEPIANativeTestReport
