Get[FileNameJoin[{DirectoryName[$InputFileName],"SEPIANative.wl"}]];
Begin["SEPIANative`Private`"];
Global`SEPIANativeZIPSessionTestReport=Module[{prompts,calls,authenticated,attested,rejected,requests,setup,response,result,old,dir,record,tests},
  setup[] := ($zipWebsite=None;$zipArchives=<||>;$zipPendingDirectories=<||>;$zipCleanupBlocked=False;
    prompts=0;calls={};authenticated=True;attested=True;rejected=False;requests=0;);
  response[data_] := <|"StatusCode"->200,"Cookies"->{},"BodyByteArray"->ExportByteArray[data,"RawJSON"]|>;
  tests=Block[{$zipWebsite,$zipArchives,$zipPendingDirectories,$zipCleanupBlocked,zipRequest,
    SEPIAZIPProbe`Private`credentials,zipDeleteDirectory,$connection,$tables,$listings,$uiArchive,$uiListing,
    $uiSelected,$uiTable,$uiMetadata,$uiBlocked,$uiStatus,$uiTone},
    SEPIAZIPProbe`Private`credentials[] := (prompts++;{"synthetic@example.invalid","synthetic-password"});
    zipRequest[method_,route_,body_:""] := (AppendTo[calls,{method,route}];Switch[route,
      "/api/auth/login",If[rejected,failure["WebsiteAccessDenied","Synthetic denial"],response[<|"uuid"->"synthetic-user","attested"->True|>]],
      "/api/public/user/session",response[<|"uuid"->If[Count[calls,{_,"/api/auth/login"}]>0 && authenticated,"synthetic-user",""],"attested"->attested|>],
      "/api/logoff",If[rejected,failure["WebsiteAccessDenied","Synthetic denial"],response[<||>]],_,failure["UnexpectedRoute","Synthetic route"]]);
    zipDeleteDirectory[d_String] := If[DirectoryQ[d],DeleteDirectory[d,DeleteContents->True];True,True];
    {
      VerificationTest[setup[];{zipEnsureWebsite[],prompts,Length[calls]},{True,1,3},TestID->"website-login-and-continuity-check"],
      VerificationTest[setup[];zipEnsureWebsite[];zipEnsureWebsite[];{prompts,Length[calls]},{1,4},TestID->"retained-website-session-reused"],
      VerificationTest[setup[];authenticated=False;{zipEnsureWebsite[][[1]],prompts},
        {"WebsiteSessionNotRetained",1},TestID->"session-loss-no-blind-retry"],
      VerificationTest[setup[];attested=False;zipEnsureWebsite[][[1]],"WebsiteAttestationRequired",TestID->"website-attestation-required"],
      VerificationTest[setup[];rejected=True;{zipEnsureWebsite[][[1]],Count[calls,{_,"/api/auth/login"}]},
        {"WebsiteAccessDenied",1},TestID->"credentials-not-retried"],
      VerificationTest[setup[];SEPIAZIPProbe`Private`credentials[] := $Canceled;result=zipEnsureWebsite[];
        SEPIAZIPProbe`Private`credentials[] := (prompts++;{"synthetic@example.invalid","synthetic-password"});
        {result,calls,$zipWebsite},{<|"Status"->"Canceled"|>,{},None},TestID->"website-dialog-cancel-no-session"],
      VerificationTest[setup[];$zipWebsite=<|"Cookies"->{},"UUID"->"synthetic-user"|>;zipCleanupResources[];
        {$zipWebsite,$zipCleanupBlocked},{None,False},TestID->"website-logout-clears-private-state"],
      VerificationTest[setup[];$zipWebsite=<|"Cookies"->{},"UUID"->"synthetic-user"|>;rejected=True;
        {zipCleanupResources[][[1]],AssociationQ[$zipWebsite],$zipCleanupBlocked},
        {"ZIPCleanupIncomplete",True,True},TestID->"failed-logout-retains-cleanup-state"],
      VerificationTest[setup[];$zipWebsite=<|"Cookies"->{},"UUID"->"synthetic-user"|>;rejected=True;zipCleanupResources[];
        rejected=False;zipCleanupResources[];{$zipWebsite,$zipCleanupBlocked},{None,False},TestID->"explicit-logout-retry-clears-state"],
      VerificationTest[setup[];dir=CreateDirectory[];$zipPendingDirectories[dir]=True;
        zipCleanupResources[];{DirectoryQ[dir],Length[$zipPendingDirectories]},{False,0},TestID->"orphaned-extraction-cleaned-on-disconnect"],
      VerificationTest[setup[];dir=CreateDirectory[];$zipPendingDirectories[dir]=True;
        Block[{zipDeleteDirectory},zipDeleteDirectory[_]:=False;result=zipCleanupResources[]];
        old={result[[1]],$zipCleanupBlocked,KeyExistsQ[$zipPendingDirectories,dir]};zipCleanupResources[];old,
        {"ZIPCleanupIncomplete",True,True},TestID->"failed-local-cleanup-tracked-for-retry"],
      VerificationTest[setup[];$zipCleanupBlocked=True;$connection="synthetic";$tables=<||>;
        uiNewSession[];uiReady[],False,TestID->"cleanup-failure-blocks-browser-actions"],
      VerificationTest[setup[];$zipCleanupBlocked=True;$connection="synthetic";
        PDSNativeOpenZIP[<|"ListingToken"->"none"|>,1][[1]],"CleanupRequired",TestID->"cleanup-required-before-new-archive"],
      VerificationTest[setup[];$zipCleanupBlocked=True;$connection="synthetic";
        PDSNativeLoadZIPMember["none",1][[1]],"CleanupRequired",TestID->"cleanup-required-before-member-load"],
      VerificationTest[Block[{PDSNativeOpenZIP},PDSNativeOpenZIP[___]:=<|"Status"->"SyntheticOpen"|>;
        uiCall["zipOpen",{"synthetic",1}]["Status"]],"SyntheticOpen",TestID->"real-ui-open-handler-resolves-public-api"],
      VerificationTest[Block[{PDSNativeLoadZIPMember},PDSNativeLoadZIPMember[___]:=<|"Status"->"SyntheticLoad"|>;
        uiCall["zipLoad",{"synthetic",1}]["Status"]],"SyntheticLoad",TestID->"real-ui-load-handler-resolves-public-api"],
      VerificationTest[Block[{PDSNativeCloseZIP},PDSNativeCloseZIP[___]:=<|"Status"->"SyntheticClose"|>;
        uiCall["zipClose",{"synthetic"}]["Status"]],"SyntheticClose",TestID->"real-ui-close-handler-resolves-public-api"],
      VerificationTest[setup[];$connection=None;PDSNativeDisconnect[]["ZIPCleanup"]["WebsiteLogout"],
        "AlreadyClosed",TestID->"idempotent-disconnect-includes-resource-status"]
    }
  ];
  TestReport[tests]
];
End[];
Global`SEPIANativeZIPSessionTestReport
