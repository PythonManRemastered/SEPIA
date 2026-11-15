uiCall["browse", {uid_, directory_, caslib_}] := PDSNativeBrowseContribution[caslib, directory];
uiCall["select", {token_, index_}] := PDSNativeOpenEntry[<|"ListingToken" -> token|>, index];
uiCall["zipOpen", {listing_, index_}] := PDSNativeOpenZIP[<|"ListingToken" -> listing|>, index];
uiCall["zipLoad", {token_, index_}] := PDSNativeLoadZIPMember[token, index];
uiCall["zipClose", {token_}] := PDSNativeCloseZIP[token];
uiCall["inspect", {token_}] := PDSNativeInspectTable[token];
uiCall["drop", {token_}] := PDSNativeDropTable[token];

If[!ValueQ[$uiNotebook], $uiNotebook = None];
If[!ValueQ[$uiImported], $uiImported = None];
If[!ValueQ[$uiBusy], $uiBusy = False];
If[!ValueQ[$uiBlocked], $uiBlocked = False];
If[!ValueQ[$uiUID], $uiUID = "Breast_EliLill_2008_168"];
$uiLargeTableCells = 1000000;
If[!ValueQ[$uiListing], $uiListing = None];
If[!ValueQ[$uiSelected], $uiSelected = 0];
If[!ValueQ[$uiArchive], $uiArchive = None];
If[!ValueQ[$uiTable], $uiTable = None];
If[!ValueQ[$uiMetadata], $uiMetadata = None];
If[!ValueQ[$uiImportSummary], $uiImportSummary = None];
If[!ValueQ[$uiTimings], $uiTimings = <||>];
If[!ValueQ[$uiStatus], $uiStatus = "Enter a SAS library name and browse its files."];
If[!ValueQ[$uiTone], $uiTone = "Info"];

uiNewSession[] := (
  $uiBlocked = False; $uiBusy = False; $uiListing = None; $uiSelected = 0;
  $uiArchive = None; $uiTable = None; $uiMetadata = None; $uiTimings = <||>;
  $uiStatus = "Connected. Enter a SAS library name and browse its files.";
  $uiTone = "Success";
);
uiDisconnected[] := (
  $uiArchive = None; $uiListing = None; $uiSelected = 0; $uiTable = None; $uiMetadata = None;
  $uiBlocked = False; $uiStatus = "Local connection closed. Any successful import remains in kernel memory.";
  $uiTone = "Info";
);
uiReady[] := $connection =!= None && !TrueQ[$uiBusy] && !TrueQ[$uiBlocked] && !TrueQ[$zipCleanupBlocked];
uiSetLibrary[value_String] := If[value =!= $uiUID,
  $uiUID = value; $uiListing = None; $uiSelected = 0; $uiMetadata = None;
  PDSNativeClearImportedData[];
  $uiStatus = "SAS library changed. Click Browse files to switch sources and release any previous temporary table.";
  $uiTone = "Info";
];
uiSetLibrary[value_String, _] := uiSetLibrary[value];
uiLibraryField[] := InputField[Dynamic[$uiUID, uiSetLibrary], String,
  FieldSize -> 42, Enabled -> Dynamic[uiReady[]]];
uiEntries[] := If[AssociationQ[$uiListing], Lookup[$uiListing, "Entries", {}], {}];
uiEntry[] := SelectFirst[uiEntries[], Lookup[#, "Index", 0] === $uiSelected &, None];
uiInZIP[] := AssociationQ[$uiListing] && Lookup[$uiListing, "ListingKind", ""] === "ZIP";
uiZIPSelected[] := !uiInZIP[] && AssociationQ[uiEntry[]] && uiKind[] === "File" && StringEndsQ[ToLowerCase[uiEntry[]["Name"]], ".zip"];
uiKind[] := If[AssociationQ[uiEntry[]], Lookup[uiEntry[], "Kind", ""], ""];
uiSupported[] := AssociationQ[uiEntry[]] && TrueQ[Lookup[uiEntry[], "SupportedImport", False]];
uiError[result_Failure] := (
  $uiTone = "Error";
  If[TrueQ[$zipCleanupBlocked] || AnyTrue[Values[$tables], Lookup[#, "State", ""] =!= "Loaded" &], $uiBlocked = True];
  $uiStatus = ToString[result[[1]], InputForm] <> ": " <>
    ToString[Lookup[result[[2]], "MessageTemplate", "Operation failed."], OutputForm];
);
uiError[_] := ($uiTone = "Error"; $uiStatus = "Unexpected response. Disconnect and reconnect before retrying."; $uiBlocked = True;);
uiNote[text_, severity_: 0] := (
  $uiStatus = text;
  $uiTone = If[severity == 1, "Warning", "Success"];
);
uiTime[label_, result_Association] := AssociateTo[$uiTimings,
  label -> Lookup[result, "ElapsedSeconds", Missing["NotAvailable"]]];
PDSNativeImportedData[] := If[AssociationQ[$uiImported], $uiImported,
  failure["NoUIImport", "No successful UI import is stored. Use Import into Wolfram first."]];
PDSNativeClearImportedData[] := ($uiImported = None; $uiImportSummary = None; <|"Status" -> "Cleared"|>);

SetAttributes[uiRun, HoldAll];
uiRun[label_, body_] := Module[{result},
  If[TrueQ[$uiBusy], Return[Null]];
  $uiTone = "Info"; $uiStatus = label;
  result = WithCleanup[$uiBusy = True, CheckAbort[
    Check[body,
      $uiBlocked = True; $uiTone = "Error";
      $uiStatus = "An unexpected Wolfram message interrupted this action. Disconnect and reconnect before retrying.";
    ],
    $uiBlocked = True; $uiTone = "Error";
    $uiStatus = "Evaluation aborted; operation outcome is uncertain. Disconnect and reconnect before retrying.";
  ], $uiBusy = False];
  Null
];


uiRelease[] := Module[{result},
  If[!AssociationQ[$uiTable], Return[True]];
  result = uiCall["drop", {$uiTable["Token"]}];
  If[FailureQ[result], uiError[result]; Return[False]];
  uiTime["Cleanup", result];
  $uiTable = None; $uiMetadata = None;
  True
];
uiAcceptListing[result_] := (
  If[FailureQ[result], uiError[result],
    $uiListing = result; $uiSelected = 0; uiTime["Browse", result];
    uiNote[ToString[Length[result["Entries"]]] <> " entries listed. Select a folder or file.", result["Severity"]]
  ]
);
uiReleaseArchive[] := Module[{result},
  If[!AssociationQ[$uiArchive], Return[True]];
  result = uiCall["zipClose", {$uiArchive["Token"]}];
  If[FailureQ[result], uiError[result]; Return[False]];
  $uiArchive = None; True
];
uiOpenZIP[] := Module[{listing = $uiListing, entry = uiEntry[], result},
  If[!uiReady[] || !uiZIPSelected[], Return[Null]];
  uiRun["Opening ZIP from the PDS website...",
    If[uiRelease[] && uiReleaseArchive[],
      result = uiCall["zipOpen", {listing["ListingToken"], entry["Index"]}];
      If[FailureQ[result], uiError[result],
        If[Lookup[result, "Status", ""] === "Canceled", uiNote["ZIP opening canceled."],
          $uiArchive = <|"Token" -> result["ArchiveToken"], "ParentListing" -> listing|>;
          PDSNativeClearImportedData[]; $uiListing = result; $uiSelected = 0;
          uiTime["ZIP", result]; uiNote["ZIP opened. Select a SAS member, then click Load on SAS."]
        ]
      ]
    ]
  ]
];
uiLeaveZIP[] := Module[{parent},
  If[!uiReady[] || !AssociationQ[$uiArchive], Return[Null]];
  parent = $uiArchive["ParentListing"];
  uiRun["Closing temporary ZIP...",
    If[uiRelease[] && uiReleaseArchive[], PDSNativeClearImportedData[]; $uiListing = parent; $uiSelected = 0;
      uiNote["ZIP closed and temporary archive deleted."]]
  ]
];
uiBrowseRoot[] := Module[{uid, result},
  If[!uiReady[], Return[Null]];
  uid = StringTrim[$uiUID];
  If[uid === "", uiError[failure["EmptyCaslib", "Enter a SAS library name."]]; Return[Null]];
  uiRun["Browsing contribution...",
    If[uiRelease[] && uiReleaseArchive[],
      $uiListing = None; $uiSelected = 0; PDSNativeClearImportedData[];
      result = uiCall["browse", {uid, "", uid}];
      uiAcceptListing[result]
    ]
  ]
];
uiNavigate[direction_] := Module[{listing = $uiListing, entry = uiEntry[], result},
  If[!uiReady[] || !AssociationQ[listing], Return[Null]];
  If[uiInZIP[], If[direction === "Up", uiLeaveZIP[]]; Return[Null]];
  If[direction === "Open" && (!AssociationQ[entry] || entry["Kind"] =!= "Directory"), Return[Null]];
  uiRun["Opening folder...",
    If[uiRelease[],
      $uiListing = None; $uiSelected = 0; PDSNativeClearImportedData[];
      result = If[direction === "Open",
        uiCall["select", {listing["ListingToken"], entry["Index"]}],
        uiCall["browse", {listing["ContributionID"], listing["ParentDirectory"], listing["Caslib"]}]
      ];
      uiAcceptListing[result]
    ]
  ]
];
uiLoad[] := Module[{listing = $uiListing, entry = uiEntry[], result, metadata},
  If[!uiReady[] || !AssociationQ[listing] || !uiSupported[], Return[Null]];
  uiRun["Loading on SAS, then inspecting metadata...",
    If[uiRelease[],
      PDSNativeClearImportedData[]; $uiMetadata = None;
      result = If[uiInZIP[], uiCall["zipLoad", {listing["ArchiveToken"], entry["Index"]}],
        uiCall["select", {listing["ListingToken"], entry["Index"]}]];
      If[FailureQ[result], uiError[result],
        If[Lookup[result, "Status", ""] === "Canceled", uiNote["ZIP member loading canceled."]; Return[Null]];
        $uiTable = result; uiTime["Load", result];
        metadata = uiCall["inspect", {result["Token"]}];
        If[FailureQ[metadata], uiError[metadata],
          $uiMetadata = metadata; uiTime["Inspect", metadata];
          uiNote["Table loaded on SAS. Metadata retrieved; patient records have not been imported.", metadata["Severity"]]
        ]
      ]
    ]
  ]
];
uiConfirmLargeImport[dimensions_, path_] := If[$FrontEnd === Null, False,
  DialogInput[Column[{
    Style["Import a large table?", 16, Bold],
    Pane[path, {460, Automatic}],
    Grid[{{"Rows:", IntegerString[dimensions[[1]]]},
      {"Columns:", IntegerString[dimensions[[2]]]},
      {"Total cells:", IntegerString[Times @@ dimensions]}}, Alignment -> Left],
    Pane["This table has more than 1,000,000 cells. Importing it may take time and use substantial memory. All rows and columns will be imported.", {460, Automatic}],
    Row[{Button["Cancel", DialogReturn[False]], Spacer[15],
      Button["Import entire table", DialogReturn[True]]}]
  }, Spacings -> 1], WindowTitle -> "SEPIA Import Confirmation"]
];

uiImport[] := Module[{result, dimensions, rows, cells},
  If[!uiReady[] || !AssociationQ[$uiTable] || !AssociationQ[$uiMetadata], Return[Null]];
  dimensions = Lookup[$uiMetadata, "Dimensions", None];
  If[!MatchQ[dimensions, {_Integer, _Integer}] || !AllTrue[dimensions, # >= 0 &],
    uiError[failure["InvalidDimensions", "The table dimensions are invalid. Reload the table before importing."]]; Return[Null]];
  rows = dimensions[[1]]; cells = Times @@ dimensions;
  uiRun["Importing into Wolfram memory...",
    If[cells > $uiLargeTableCells && !TrueQ[uiConfirmLargeImport[dimensions, $uiTable["RelativePath"]]],
      $uiStatus = "Import canceled. No records were fetched."; $uiTone = "Info"; Return[Null]];
    PDSNativeClearImportedData[];
    result = PDSNativeFetchTable[$uiTable["Token"], "MaxRows" -> Max[1, rows], "MaxCells" -> Max[1, cells]];
    If[FailureQ[result], uiError[result],
      $uiImported = result;
      $uiImportSummary = <|"Source" -> result["Provenance"]["RelativePath"],
        "Contribution" -> Lookup[result["Provenance"], "ContributionID", result["Provenance"]["Caslib"]],
        "Dimensions" -> result["Dimensions"], "Severity" -> result["Severity"]|>;
      uiTime["Import", result];
      uiNote["Import complete. Use data = PDSNativeImportedData[]; in your notebook to access it.", result["Severity"]]
    ]
  ]
];
uiDrop[] := If[uiReady[], uiRun["Removing temporary table...",
  If[uiRelease[], uiNote["Temporary table removed. A successful local import, if any, remains available."]]
]];
uiDisconnect[] := Module[{result, notebook = $uiNotebook},
  If[TrueQ[$uiBusy], Return[Null]];
  uiRun["Disconnecting from SAS...",
    result = PDSNativeDisconnect[];
    If[FailureQ[result], uiError[result],
      $uiNotebook = None;
      If[MatchQ[notebook, _NotebookObject], NotebookClose[notebook]]
    ]
  ]
];

uiFileChoices[] := If[uiEntries[] === {}, {0 -> "No entries available"},
  Prepend[(# ["Index"] -> (# ["Name"] <> "  [" <>
    If[# ["Kind"] === "Directory", "folder", If[TrueQ[# ["SupportedImport"]], "SAS table", If[StringEndsQ[ToLowerCase[# ["Name"]], ".zip"] && !uiInZIP[], "ZIP archive", "unsupported"]]] <> "]")) & /@ uiEntries[],
    0 -> "Select a file or folder..."]];
uiFileMenu[] := Dynamic[
  PopupMenu[Dynamic[$uiSelected], uiFileChoices[], FieldSize -> 65,
    Enabled -> Dynamic[uiReady[] && AssociationQ[$uiListing]]],
  TrackedSymbols :> {$uiListing}
];
uiStatusColor[] := Switch[$uiTone, "Error", RGBColor[.72,.15,.16], "Warning", RGBColor[.65,.4,.05],
  "Success", RGBColor[.1,.42,.32], _, RGBColor[.2,.3,.43]];
uiTablePanel[] := If[!AssociationQ[$uiTable], Style["No table loaded.", Gray],
  Column[{
    Style[$uiTable["RelativePath"], Bold],
    Row[{"Contribution: ", Lookup[$uiTable, "ContributionID", $uiTable["Caslib"]]}],
    Row[{"SAS library: ", $uiTable["Caslib"]}],
    If[AssociationQ[$uiMetadata],
      Column[{
        Row[{"Rows: ", $uiMetadata["Dimensions"][[1]], "     Columns: ", $uiMetadata["Dimensions"][[2]],
          "     Load status: ", $uiMetadata["LoadStatus"], " (", $uiMetadata["LoadSeverity"], ")"}],
        Style["Column names", Bold],
        Pane[Column[$uiMetadata["ColumnNames"], Spacings -> .2], {740, 130}, Scrollbars -> True]
      }],
      Style["Table exists, but metadata inspection did not complete. Drop the table before retrying.", Darker[Orange]]
    ]
  }, Spacings -> .6]
];
uiHint[text_] := Pane[Style[text, 11, Gray], {750, Automatic}];
uiContents[] := Column[{
  Style["SEPIA Data Browser", 24, Bold, RGBColor[.12,.24,.34]],
  Panel[Dynamic[Style[$uiStatus, uiStatusColor[]]], ImageSize -> {770, Automatic}],
  Style["1  Choose a SAS library", 15, Bold],
  Row[{"SAS library name:  ", uiLibraryField[]}],
  Button["Browse files", uiBrowseRoot[], Method -> "Queued", Enabled -> Dynamic[uiReady[]]],
  Style["2  Browse and select", 15, Bold],
  Dynamic[If[AssociationQ[$uiListing], Row[{"Browsing: ", $uiListing["ContributionID"], " / ",
    If[$uiListing["Directory"] === "", "(root)", $uiListing["Directory"]], If[uiInZIP[], " (ZIP members)", ""]}], "No folder listing."]],
  uiFileMenu[],
  Dynamic[If[AssociationQ[uiEntry[]], Row[{"Type: ", uiEntry[]["Kind"], "     Bytes: ", uiEntry[]["Bytes"],
    "     Path: ", uiEntry[]["RelativePath"]}], ""]],
  Row[{
    Button["Up one folder", uiNavigate["Up"], Method -> "Queued",
      Enabled -> Dynamic[uiReady[] && AssociationQ[$uiListing] && $uiListing["Directory"] =!= ""]], Spacer[10],
    Button["Open folder", uiNavigate["Open"], Method -> "Queued", Enabled -> Dynamic[uiReady[] && !uiInZIP[] && uiKind[] === "Directory"]], Spacer[10],
    Button["Open ZIP", uiOpenZIP[], Method -> "Queued", Enabled -> Dynamic[uiReady[] && uiZIPSelected[]]], Spacer[10],
    Button["Load on SAS", uiLoad[], Method -> "Queued", Enabled -> Dynamic[uiReady[] && uiSupported[]]]
  }],
  uiHint["Open ZIP to select a SAS member inside it. PDS website login is requested when needed. PDFs remain listed only. Up one folder closes a ZIP and deletes its temporary copy."],
  Style["3  Table information", 15, Bold],
  Dynamic[uiTablePanel[]],
  Style["4  Optional import into Wolfram", 15, Bold],
  uiHint["Import retrieves the complete table automatically. Tables with more than 1,000,000 cells require confirmation. Records are transferred to this Mac and are not displayed here."],
  Row[{
    Button["Import into Wolfram", uiImport[], Method -> "Queued",
      Enabled -> Dynamic[uiReady[] && AssociationQ[$uiTable] && AssociationQ[$uiMetadata]]], Spacer[10],
    Button["Drop temporary table", uiDrop[], Method -> "Queued", Enabled -> Dynamic[uiReady[] && AssociationQ[$uiTable]]], Spacer[10],
    Button["Clear local import", PDSNativeClearImportedData[], Method -> "Queued", Enabled -> Dynamic[!$uiBusy && AssociationQ[$uiImported]]]
  }],
  Dynamic[If[AssociationQ[$uiImportSummary], Row[{"Stored import: ", $uiImportSummary["Contribution"], " / ", $uiImportSummary["Source"],
      "   ", $uiImportSummary["Dimensions"], "   severity ", $uiImportSummary["Severity"]}], "No UI import stored."]],
  Dynamic[If[Length[$uiTimings] > 0, Row[{"Last completed timings (seconds): ",
    Row[KeyValueMap[Row[{#1, " ", #2}] &, $uiTimings], "   |   "]}], ""]],
  uiHint["Status: 0 = success; 1 = warning. Errors are shown above. Timings belong to the last completed action of each type."],
  Button["Disconnect and close", uiDisconnect[], Method -> "Queued", Enabled -> Dynamic[!$uiBusy]],
  uiHint["Use Disconnect and close to end the SAS session. Closing only the window leaves the connection open; reopen with PDSNativeOpenBrowser[]."]
}, Alignment -> Left, Spacings -> .8, BaseStyle -> {FontFamily -> "Helvetica", FontSize -> 12}];

PDSNativeOpenBrowser[] := Module[{},
  If[$connection === None, Return[failure["NotConnected", "Run PDSNativeConnect[] first."]]];
  If[$FrontEnd === Null, Return[failure["NoFrontEnd", "The browser requires desktop Mathematica."]]];
  If[MatchQ[$uiNotebook, _NotebookObject] && MemberQ[Notebooks[], $uiNotebook],
    SetSelectedNotebook[$uiNotebook]; Return[$uiNotebook]];
  $uiNotebook = CreateDialog[
    Pane[uiContents[], ImageSize -> {800, 780}, Scrollbars -> {False, True}],
    WindowTitle -> "SEPIA Data Browser", WindowSize -> {850, 850},
    Saveable -> False, WindowFloating -> False,
    NotebookEventActions -> {"WindowClose" :> ($uiNotebook = None)}
  ];
  $uiNotebook
];
