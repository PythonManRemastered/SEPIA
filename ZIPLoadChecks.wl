Module[{directory = DirectoryName[$InputFileName]},
  Get[FileNameJoin[{directory,"SEPIANative.wl"}]];
  Get[FileNameJoin[{directory,"SEPIANativeUpload.wl"}]];
  Get[FileNameJoin[{directory,"SEPIAZIPProbe.wl"}]];
  Get[FileNameJoin[{directory,"SEPIAZIPLoadCheck.wl"}]];
  Get[FileNameJoin[{directory,"ZIPLoadFixtures.wl"}]]
];
Begin["SEPIAZIPLoadCheck`Private`"];
Global`SEPIAZIPLoadTestReport = Module[{paths = <||>, extractionDirectories = {}, fixtureDirectory, tests, makeZIP, extraction,
    process, nativeBytes = $syntheticBytes, source, mockConnection, nativeUpload, validResult},
  fixtureDirectory = CreateDirectory[];
  makeZIP[key_] := Module[{path = FileNameJoin[{fixtureDirectory,key <> ".zip"}]},
    SEPIAZIPProbe`Private`writeBytes[path,$syntheticZIPs[key]]; path];
  Scan[(paths[#] = makeZIP[#]) &, Keys[$syntheticZIPs]];
  extraction[key_, hash_: Automatic] := Module[{d = CreateDirectory[], result, names},
    AppendTo[extractionDirectories,d];
    names = SEPIAZIPProbe`Private`zipNames[paths[key]];
    result = Block[{$memberBytes = Length[$syntheticBytes], $memberSHA256 = If[hash===Automatic,$syntheticHash,hash]},
      extractMember[paths[key],names,d]];
    {result,FileNameTake /@ FileNames[All,d,Infinity]}
  ];
  process[inspectResult_, uploadResult_: Automatic, dropResult_: Automatic] := Block[
    {$memberBytes = Length[$syntheticBytes], $memberSHA256 = $syntheticHash, uploadCall,inspectCall,dropCall,Print},
    uploadCall[bytes_, provenance_] := If[uploadResult===Automatic,<|"Token"->"synthetic-token","LoadSeverity"->0|>,uploadResult];
    inspectCall[_] := inspectResult;
    dropCall[_] := If[dropResult===Automatic,<|"Status"->"Dropped","Severity"->0|>,dropResult];
    processMember[paths["ordinary"],{"adsl.sas7bdat","other.txt"}]
  ];
  validResult = <|"Dimensions"->{385,110},"ColumnNames"->{"synthetic"},"Severity"->0|>;
  source = <|"ContributionID"->"synthetic-study","SourceArchive"->"synthetic.zip","ArchiveMember"->"adsl.sas7bdat"|>;
  mockConnection = <|"Session"->"11111111-2222-3333-4444-555555555555","Authorization"->"Basic synthetic","Affinity"->"synthetic"|>;
  nativeUpload[answer_] := Block[{SEPIANative`Private`$connection = mockConnection,
    SEPIANative`Private`$tables = <||>,SEPIANative`Private`request},
    SEPIANative`Private`request[method_,route_,bytes_,auth_,affinity_,timeout_,headers_] := answer;
    {SEPIANative`PDSNativeUploadSAS[nativeBytes,source],Values[SEPIANative`Private`$tables]}
  ];
  tests = {
    VerificationTest[Block[{$memberBytes=Length[$syntheticBytes]},memberDirectory[paths["ordinary"]]["Bytes"]],Length[$syntheticBytes],TestID->"central-directory-size"],
    VerificationTest[Block[{$memberBytes=Length[$syntheticBytes]},FailureQ[memberDirectory[paths["wrongsize"]]]],True,TestID->"wrong-expanded-size-before-extraction"],
    VerificationTest[Block[{$memberBytes=Length[$syntheticBytes]},FailureQ[memberDirectory[paths["symlink"]]]],True,TestID->"symlink-member-rejected"],
    VerificationTest[Block[{$memberBytes=Length[$syntheticBytes]},FailureQ[memberDirectory[paths["encryptedflag"]]]],True,TestID->"encrypted-member-rejected"],
    VerificationTest[extraction["ordinary"][[2]],{"adsl.sas7bdat"},TestID->"only-selected-member-extracted"],
    VerificationTest[AssociationQ[extraction["ordinary"][[1]]],True,TestID->"exact-synthetic-hash"],
    VerificationTest[FailureQ[extraction["ordinary","incorrect"][[1]]],True,TestID->"changed-hash-no-upload"],
    VerificationTest[extraction["traversal"][[2]],{},TestID->"traversal-archive-no-extraction"],
    VerificationTest[extraction["duplicate"][[2]],{},TestID->"ambiguous-member-no-extraction"],
    VerificationTest[KeyTake[process[validResult],{"Status","MembersExtracted","MatchesExpectedDimensions","ExtractedDirectoryDeleted"}],
      <|"Status"->"ZIPMemberInspected","MembersExtracted"->1,"MatchesExpectedDimensions"->True,"ExtractedDirectoryDeleted"->True|>,TestID->"member-pipeline-and-local-cleanup"],
    VerificationTest[process[validResult]["TableCleanup"]["Status"],"Dropped",TestID->"successful-upload-table-dropped"],
    VerificationTest[process[Failure["SyntheticInspectionFailure",<||>]][[2]]["ExtractedDirectoryDeleted"],True,TestID->"inspection-failure-local-cleanup"],
    VerificationTest[process[Failure["SyntheticInspectionFailure",<||>]][[2]]["TableCleanup"]["Status"],"Dropped",TestID->"inspection-failure-server-cleanup"],
    VerificationTest[process[<|"Dimensions"->{1,2}|>][[1]],"DimensionsMismatch",TestID->"wrong-dimensions-rejected"],
    VerificationTest[process[validResult,Failure["SyntheticUploadFailure",<||>]][[2]]["TableCleanup"]["Status"],"NotCreated",TestID->"unconfirmed-upload-no-unsafe-drop"],
    VerificationTest[process[validResult,Automatic,Failure["DropUnconfirmed",<||>]]["TableCleanup"][[1]],"DropUnconfirmed",TestID->"drop-failure-reported"],
    VerificationTest[nativeUpload[<|"Payload"-><|"disposition"-><|"severity"->"Normal"|>|>,"Affinity"->"synthetic"|>][[1]]["Status"],"Loaded",TestID->"native-binary-upload-success"],
    VerificationTest[nativeUpload[Failure["SyntheticTimeout",<||>]][[2,1]]["State"],"LoadUnconfirmed",TestID->"timeout-retains-unconfirmed-record"],
    VerificationTest[nativeUpload[<|"Payload"-><||>,"Affinity"->"synthetic"|>][[1,1]],"UploadUnconfirmed",TestID->"missing-disposition-not-success"],
    VerificationTest[nativeUpload[<|"Payload"-><|"disposition"-><|"severity"->"Error"|>|>,"Affinity"->"synthetic"|>][[2,1]]["State"],"LoadUnconfirmed",TestID->"server-error-retains-record"],
    VerificationTest[Block[{SEPIANative`Private`$connection=None},SEPIANative`PDSNativeUploadSAS[nativeBytes,source][[1]]],"NotConnected",TestID->"upload-requires-session"],
    VerificationTest[Block[{SEPIANative`Private`$connection=mockConnection},SEPIANative`PDSNativeUploadSAS[ByteArray[{1,2}],source][[1]]],"NotSAS7BDAT",TestID->"non-sas-bytes-rejected"],
    VerificationTest[Block[{SEPIANative`Private`$connection=mockConnection},SEPIANative`PDSNativeUploadSAS[nativeBytes,Append[source,"ArchiveMember"->"../adsl.sas7bdat"]][[1]]],"InvalidUploadSource",TestID->"upload-source-traversal-rejected"],
    VerificationTest[Block[{SEPIANative`Private`$connection=mockConnection,SEPIANative`Private`$tables=<|"old"-><|"State"->"LoadUnconfirmed"|>|>},
      SEPIANative`PDSNativeUploadSAS[nativeBytes,source][[1]]],"CleanupRequired",TestID->"unresolved-upload-blocks-retry"],
    VerificationTest[Block[{SEPIANative`Private`$connection=mockConnection},PDSZIPLoadCheck[][[1]]],"ExistingSASSession",TestID->"existing-browser-session-preserved"],
    VerificationTest[Block[{SEPIANative`Private`$connection=mockConnection,
      SEPIANative`Private`$tables=<||>,SEPIANative`Private`request},
      SEPIANative`Private`request[method_,route_,bytes_,auth_,affinity_,timeout_,headers_] :=
        If[method === "PUT" && StringEndsQ[route,"/actions/table.upload"] && bytes === nativeBytes &&
          !StringContainsQ[headers["JSON-Parameters"],{"\n","\r"}] &&
          ImportString[headers["JSON-Parameters"],"RawJSON"]["importOptions"]["fileType"] === "basesas",
          <|"Payload"-><|"disposition"-><|"severity"->"Normal"|>|>,"Affinity"->"synthetic"|>,Failure["InvalidUploadRequest",<||>]];
      SEPIANative`PDSNativeUploadSAS[nativeBytes,source]["Status"]],"Loaded",TestID->"upload-route-single-line-json-parameters-and-original-bytes"],
    VerificationTest[Block[{SEPIANative`Private`$connection=None,connectCall,disconnectCall,SEPIAZIPProbe`Private`credentials},
      connectCall[] := <|"Status"->"Connected"|>;
      disconnectCall[] := <|"Status"->"Closed","ServerCleanup"->"Confirmed"|>;
      SEPIAZIPProbe`Private`credentials[] := $Canceled;
      PDSZIPLoadCheck[]["SASDisconnect"]["Status"]],"Closed",TestID->"website-login-cancel-closes-owned-sas-session"],
    VerificationTest[Block[{SEPIANative`Private`transport},
      SEPIANative`Private`transport[req_,timeout_] := If[req["Method"]==="PUT" && req["BodyByteArray"]===nativeBytes &&
        Lookup[Association[req["Headers"]],"content-type",""]==="application/octet-stream" &&
        Lookup[Association[req["Headers"]],"json-parameters",""]==="synthetic-parameters",
        <|"StatusCode"->200,"BodyByteArray"->ExportByteArray[<||>,"RawJSON"],"Cookies"->{}|>,$Failed];
      AssociationQ[SEPIANative`Private`request["PUT","synthetic",nativeBytes,"synthetic","",60,
        <|"Content-Type"->"application/octet-stream","JSON-Parameters"->"synthetic-parameters"|>]]],True,TestID->"raw-request-binary-body-and-upload-headers"]
  };
  WithCleanup[Null,TestReport[tests],
    Scan[deleteDirectory,extractionDirectories];deleteDirectory[fixtureDirectory]]
];
End[];
Global`SEPIAZIPLoadTestReport
