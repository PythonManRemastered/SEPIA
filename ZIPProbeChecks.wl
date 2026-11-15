Get[FileNameJoin[{DirectoryName[$InputFileName], "SEPIAZIPProbe.wl"}]];
Begin["SEPIAZIPProbe`Private`"];
Global`SEPIAZIPProbeTestReport = Module[{bytes = BaseDecode["UEsDBBQAAAAAAKmLbl3dr4CZDgAAAA4AAAAaAAAAYWRhbTIwMTYxMTAzL2Fkc2wuc2FzN2JkYXRzeW50aGV0aWMgb25seVBLAQIUAxQAAAAAAKmLbl3dr4CZDgAAAA4AAAAaAAAAAAAAAAAAAACAAQAAAABhZGFtMjAxNjExMDMvYWRzbC5zYXM3YmRhdFBLBQYAAAAAAQABAEgAAABGAAAAAAA="], metadata, response, scenario, tests},
 metadata = <|"uniqueDatasetId" -> $studyUID, "uuid" -> $studyUUID, "donationId" -> 168,
   "fileGroups" -> {<|"files" -> {<|"filename" -> $archiveName, "downloadable" -> True,
     "id" -> 1003, "donationId" -> 168, "fileSize" -> Length[bytes]|>}|>}|>;
 response[value_] := <|"StatusCode" -> 200, "Cookies" -> {<|"Name" -> "synthetic", "Content" -> "synthetic"|>},
   "BodyByteArray" -> ExportByteArray[value, "RawJSON"]|>;
 scenario[login_, meta_, binary_, session_: Automatic] := Module[{calls = {}, result},
   result = Block[{request, $expectedBytes = Length[bytes]},
     request[method_, route_, body_, cookies_] := (AppendTo[calls, route];
       Switch[route, "/api/auth/login", response[login],
         "/api/public/user/session", response[If[Count[calls, "/api/public/user/session"] === 1,
           <|"uuid" -> "", "attested" -> False|>, If[session === Automatic, login, session]]],
         "/api/accessdata/results/" <> $studyUUID, response[meta],
         "/api/file/download/168/1003", <|"StatusCode" -> 200, "Cookies" -> {}, "BodyByteArray" -> binary|>,
         "/api/logoff", response[<||>], _, fail["UnexpectedRoute", "Unexpected route."]]);
     Block[{Print}, runCheck[{"synthetic@example.invalid", "synthetic password"}]]
   ];
   {result, Count[calls, "/api/file/download/168/1003"]}
 ];
 tests = {
   VerificationTest[runCheck[$Canceled], <|"Status" -> "Canceled", "DownloadAttempted" -> False|>, TestID -> "cancel-no-request"],
   VerificationTest[FailureQ[runCheck[{"", "x"}]], True, TestID -> "empty-login-rejected"],
   VerificationTest[AllTrue[{"../escape", "/absolute", "C:/windows", "a/../b", "a\\b", "a" <> FromCharacterCode[0]}, !safeMember[#]&],
     True, TestID -> "unsafe-member-paths"],
   VerificationTest[safeMember["folder/ordinary file.sas7bdat"], True, TestID -> "ordinary-member"],
   VerificationTest[FailureQ[checkNames[{"A.sas7bdat", "a.sas7bdat"}]], True, TestID -> "ambiguous-case-members"],
   VerificationTest[FailureQ[checkNames[$Failed]], True, TestID -> "invalid-directory"],
   VerificationTest[FailureQ[archiveEntry[<||>]], True, TestID -> "wrong-contribution-blocked"],
   VerificationTest[Block[{$expectedBytes = Length[bytes]}, AssociationQ[archiveEntry[metadata]]], True, TestID -> "verified-source-entry"],
   VerificationTest[scenario[<|"uuid" -> "", "attested" -> True|>, metadata, bytes][[2]], 0, TestID -> "guest-no-download"],
   VerificationTest[scenario[<|"uuid" -> "synthetic", "attested" -> False|>, metadata, bytes][[2]], 0, TestID -> "no-auto-attestation"],
   VerificationTest[scenario[<|"uuid" -> "synthetic", "attested" -> True|>,
     ReplacePart[metadata, {"fileGroups", 1, "files", 1, "downloadable"} -> False], bytes][[2]],
     0, TestID -> "permission-flag-enforced"],
   VerificationTest[scenario[<|"uuid" -> "synthetic", "attested" -> True|>, metadata,
     ByteArray[{80,75}]][[1,1]], "ArchiveLengthMismatch", TestID -> "truncated-download-rejected"],
   VerificationTest[scenario[<|"uuid" -> "synthetic", "attested" -> True|>, metadata,
     ByteArray[ConstantArray[60, Length[bytes]]]][[1,1]], "NotZIP", TestID -> "html-not-treated-as-zip"],
   VerificationTest[KeyTake[scenario[<|"uuid" -> "synthetic", "attested" -> True|>, metadata, bytes][[1]],
     {"Status", "Members", "MembersExtracted", "TemporaryFileDeleted", "WebsiteLogout"}],
     <|"Status" -> "ZIPListed", "Members" -> {"adam20161103/adsl.sas7bdat"}, "MembersExtracted" -> 0,
       "TemporaryFileDeleted" -> True, "WebsiteLogout" -> "Confirmed"|>, TestID -> "real-synthetic-zip-list-and-cleanup"],
   VerificationTest[Block[{transport}, transport[_] := <|"StatusCode" -> 302|>;
     request["GET", "/api/public/user/session", "", {}][[1]]], "WebsiteRedirect", TestID -> "redirect-not-followed"],
   VerificationTest[Block[{transport}, transport[_] := <|"StatusCode" -> 403|>;
     request["GET", "/api/public/user/session", "", {}][[1]]], "WebsiteAccessDenied", TestID -> "access-denial"],
   VerificationTest[Block[{transport}, transport[_] := $Failed;
     request["GET", "/api/public/user/session", "", {}][[1]]], "TransportFailure", TestID -> "transport-failure"],
   VerificationTest[scenario[<|"uuid" -> "synthetic", "attested" -> True|>, metadata, bytes,
      <|"uuid" -> "", "attested" -> False|>][[1,1]], "WebsiteSessionNotRetained", TestID -> "guest-after-login-detected"],
   VerificationTest[scenario[<|"uuid" -> "synthetic", "attested" -> True|>, metadata, bytes,
      <|"uuid" -> "other", "attested" -> True|>][[2]], 0, TestID -> "different-session-no-download"],
   VerificationTest[scenario[<|"uuid" -> "synthetic", "attested" -> True|>, metadata, bytes,
      <|"uuid" -> "synthetic", "attested" -> False|>][[2]], 0, TestID -> "session-attestation-enforced"],
   VerificationTest[scenario[<|"uuid" -> "synthetic", "attested" -> True|>, metadata, bytes][[1]]["Diagnostics"]["SessionMatchesLogin"],
      True, TestID -> "session-continuity-confirmed"],
   VerificationTest[FreeQ[scenario[<|"uuid" -> "synthetic", "attested" -> True|>, metadata, bytes][[1]],
      "synthetic password" | "synthetic@example.invalid" | "synthetic"], True, TestID -> "no-account-or-cookie-values-in-result"],
   VerificationTest[KeyExistsQ[scenario[<|"uuid" -> "synthetic", "attested" -> True|>, metadata, bytes,
      <|"uuid" -> "", "attested" -> False|>][[1,2]], "Diagnostics"], True, TestID -> "failure-retains-diagnostics"],
   VerificationTest[scenario[<|"uuid" -> "synthetic", "attested" -> True|>, metadata, bytes,
      <|"uuid" -> "", "attested" -> False|>][[1,2]]["WebsiteLogout"], "Confirmed", TestID -> "early-exit-reports-cleanup"],
   VerificationTest[Block[{performHTTP},
     performHTTP[req_, capture_] := (capture[<|"Name" -> "session", "Content" -> "synthetic",
       "Domain" -> "data.projectdatasphere.org", "Path" -> "/"|>];
       <|"StatusCode" -> 200, "Cookies" -> {}, "BodyByteArray" -> ByteArray[{}]|>);
     Length[transport[HTTPRequest[$host]]["Cookies"]]], 1, TestID -> "callback-captures-cookie-without-global-storage"],
   VerificationTest[Block[{performHTTP},
     performHTTP[req_, capture_] := (capture[<|"Name" -> "other", "Content" -> "synthetic",
       "Domain" -> "unrelated.invalid", "Path" -> "/"|>];
       <|"StatusCode" -> 200, "Cookies" -> {<|"Name" -> "other", "Content" -> "synthetic",
         "Domain" -> "unrelated.invalid", "Path" -> "/"|>}|>);
     transport[HTTPRequest[$host]]["Cookies"]], {}, TestID -> "unrelated-cookies-excluded"],
   VerificationTest[Block[{performHTTP},
     performHTTP[req_, capture_] := (capture[<|"Name" -> "session", "Content" -> "synthetic",
       "Domain" -> "data.projectdatasphere.org", "Path" -> "/"|>];
       <|"StatusCode" -> 200, "Cookies" -> {<|"Name" -> "session", "Content" -> "synthetic",
         "Domain" -> "data.projectdatasphere.org", "Path" -> "/"|>}|>);
     Length[transport[HTTPRequest[$host]]["Cookies"]]], 1, TestID -> "callback-and-response-deduplicated"],
   VerificationTest[mergeCookies[{<|"Name" -> "a", "Content" -> "old"|>},
      <|"Cookies" -> {<|"Name" -> "a", "Content" -> "new"|>}|>],
     {<|"Name" -> "a", "Content" -> "new"|>}, TestID -> "updated-session-cookie"],
   VerificationTest[Block[{$expectedBytes = Length[bytes]},
     FailureQ[archiveEntry[ReplacePart[metadata, {"fileGroups",1,"files",1,"fileSize"} -> 1]]]],
     True, TestID -> "changed-archive-no-download"]
 };
 TestReport[tests]
];
End[];
Global`SEPIAZIPProbeTestReport

