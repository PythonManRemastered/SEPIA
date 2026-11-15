Get[FileNameJoin[{DirectoryName[$InputFileName], "SEPIANative.wl"}]];
Begin["SEPIANative`Private`"];
Global`SEPIANativeBrowserTestReport = Module[{tests, listing, loaded, metadata, fetched, calls, mock, setup, result, setter},
 listing = <|"ContributionID" -> "SYNTHETIC", "Caslib" -> "SYNTHETIC", "Directory" -> "", "ParentDirectory" -> "",
   "ListingToken" -> "list1", "Severity" -> 0, "ElapsedSeconds" -> .1,
   "Entries" -> {<|"Index" -> 1, "Name" -> "adsl.sas7bdat", "RelativePath" -> "adsl.sas7bdat", "Kind" -> "File", "Bytes" -> 100, "SupportedImport" -> True|>,
                  <|"Index" -> 2, "Name" -> "guide.pdf", "RelativePath" -> "guide.pdf", "Kind" -> "File", "Bytes" -> 10, "SupportedImport" -> False|>}|>;
 loaded = <|"Token" -> "table1", "ContributionID" -> "SYNTHETIC", "Caslib" -> "SYNTHETIC", "RelativePath" -> "adsl.sas7bdat",
   "Rows" -> 2, "Columns" -> 2, "LoadSeverity" -> 0, "ElapsedSeconds" -> .2|>;
 metadata = <|"Dimensions" -> {2,2}, "ColumnNames" -> {"ID", "VALUE"}, "LoadStatus" -> "Success", "LoadSeverity" -> 0, "Severity" -> 0, "ElapsedSeconds" -> .1|>;
 fetched = <|"Dimensions" -> {2,2}, "ColumnNames" -> {"ID", "VALUE"}, "Data" -> {{"a",1},{"b",2}},
   "Provenance" -> loaded, "Severity" -> 0, "ElapsedSeconds" -> .3|>;
 setup[] := ($tables = <||>; $connection = "mock"; uiNewSession[]; PDSNativeClearImportedData[]; $uiListing = listing; $uiSelected = 1; $uiUID = "SYNTHETIC"; $uiCaslib = ""; calls = {};);
 mock[op_, args_] := (AppendTo[calls, op]; Switch[op, "browse", listing, "select", loaded, "inspect", metadata,
   "drop", <|"Status" -> "Dropped", "Severity" -> 0, "ElapsedSeconds" -> .1|>, _, failure["Unexpected", "Unexpected mock call"]]);
 tests = Block[{uiCall, $tables = <||>, PDSNativeFetchTable, $connection, $uiNotebook = None, $uiListing, $uiSelected, $uiTable, $uiMetadata, $uiImported,
   $uiImportSummary, $uiTimings, $uiStatus, $uiTone, $uiBusy, $uiBlocked, $uiUID, $uiCaslib},
  uiCall[op_, args_] := mock[op,args];
  PDSNativeFetchTable[___] := fetched;
  {
   VerificationTest[setup[]; uiLoad[]; {$uiTable["Token"], $uiMetadata["Dimensions"], calls}, {"table1",{2,2},{"select","inspect"}}, TestID -> "load-inspect-without-fetch"],
   VerificationTest[setup[]; $uiSelected=2; uiLoad[]; calls, {}, TestID -> "unsupported-selection-does-not-load"],
   VerificationTest[setup[]; uiLoad[]; uiImport[]; {PDSNativeImportedData[]["Dimensions"], $uiImportSummary["Source"]}, {{2,2},"adsl.sas7bdat"}, TestID -> "import-accessor"],
   VerificationTest[setup[]; uiLoad[]; uiImport[]; uiBrowseRoot[]; {FailureQ[PDSNativeImportedData[]], $uiTable, calls}, {True,None,{"select","inspect","drop","browse"}}, TestID -> "new-browse-cleans-and-clears"],
   VerificationTest[setup[]; uiLoad[]; uiCall["drop", _] := failure["DropFailed","failed"]; uiBrowseRoot[];
     result={$uiTable["Token"],$uiTone,Count[calls,"browse"]}; Clear[uiCall]; uiCall[op_,args_] := mock[op,args]; result,
     {"table1","Error",0}, TestID -> "cleanup-failure-stops-replacement"],
   VerificationTest[setup[]; uiLoad[]; uiCall["select", _] := failure["LoadFailed","failed"]; uiLoad[];
     result={$uiTable,$uiMetadata,$uiTone}; Clear[uiCall]; uiCall[op_,args_] := mock[op,args]; result,
     {None,None,"Error"}, TestID -> "failed-new-load-clears-old-metadata"],
   VerificationTest[setup[]; uiCall["inspect", _] := failure["InspectFailed","failed"]; uiLoad[];
     result={AssociationQ[$uiTable],$uiMetadata,$uiTone}; Clear[uiCall]; uiCall[op_,args_] := mock[op,args]; result,
     {True,None,"Error"}, TestID -> "metadata-failure-keeps-cleanup-token"],
   VerificationTest[setup[]; uiLoad[]; uiImport[]; uiDrop[]; {$uiTable,AssociationQ[PDSNativeImportedData[]]},
     {None,True}, TestID -> "drop-preserves-local-import"],
   VerificationTest[setup[]; uiLoad[]; $uiMetadata = Join[$uiMetadata, <|"Dimensions" -> {-1, 2}|>]; uiImport[]; {FailureQ[PDSNativeImportedData[]],$uiTone},
     {True,"Error"}, TestID -> "invalid-ui-dimensions"],
   VerificationTest[setup[]; $uiBusy=True; uiLoad[]; calls, {}, TestID -> "busy-prevents-load"],
   VerificationTest[setup[]; uiRun["test",Abort[]]; {$uiBusy,$uiBlocked,$uiTone}, {False,True,"Error"}, TestID -> "abort-blocks-further-actions"],
   VerificationTest[setup[]; uiLoad[]; uiImport[]; uiDisconnected[]; {$uiTable,$uiListing,AssociationQ[PDSNativeImportedData[]]},
     {None,None,True}, TestID -> "disconnect-clears-remote-state"],
   VerificationTest[setup[]; Head[uiContents[]], Column, TestID -> "ui-expression-builds"],
   VerificationTest[setup[]; !FreeQ[ToBoxes[uiFileMenu[][[1]]], _PopupMenuBox], True, TestID -> "populated-menu-renders-control"],
   VerificationTest[setup[]; $uiListing=None; !FreeQ[ToBoxes[uiFileMenu[][[1]]], _PopupMenuBox], True, TestID -> "empty-menu-renders-control"],
   VerificationTest[Options[PDSNativeConnect], {"Timeout" -> 60, "OpenUI" -> False}, TestID -> "command-only-default-option"],
   VerificationTest[PDSNativeConnect["OpenUI" -> "bad"][[1]], "InvalidOption", TestID -> "invalid-ui-option"],
   VerificationTest[setup[]; setter = uiLibraryField[][[1, 2]];
     setter["Multiple_Brigham_454", $uiUID]; {$uiUID, $uiSelected, $uiListing},
     {"Multiple_Brigham_454", 0, None}, TestID -> "input-field-two-argument-setter"],
   VerificationTest[setup[]; setter = uiLibraryField[][[1, 2]];
     setter["", $uiUID]; setter["M", $uiUID]; setter["Multiple_Brigham_454", $uiUID];
     {$uiUID, calls}, {"Multiple_Brigham_454", {}}, TestID -> "typing-library-persists-without-network-calls"],
   VerificationTest[setup[]; uiLoad[]; uiImport[]; setter = uiLibraryField[][[1, 2]];
     setter["Multiple_Brigham_454", $uiUID];
     {$uiUID, FailureQ[PDSNativeImportedData[]], $uiMetadata, AssociationQ[$uiTable]},
     {"Multiple_Brigham_454", True, None, True}, TestID -> "input-field-edit-invalidates-stale-state"]
  }
 ];
 TestReport[tests]
];
End[];
Global`SEPIANativeBrowserTestReport
