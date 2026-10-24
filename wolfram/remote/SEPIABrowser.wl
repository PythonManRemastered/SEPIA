If[!ValueQ[$uiNotebook], $uiNotebook = None];
If[!ValueQ[$uiImported], $uiImported = None];
$uiBusy = False;
$uiBlocked = False;
$uiUID = "Breast_EliLill_2008_168";
$uiRowLimit = 1000;
$uiCellLimit = 200000;
$uiListing = None;
$uiSelected = 0;
$uiTable = None;
$uiMetadata = None;
$uiImportSummary = None;
$uiTimings = <||>;
$uiStatus = "Enter a SAS library name and browse its files.";
$uiTone = "Info";

uiNewSession[] := (
  $uiBlocked = False; $uiBusy = False; $uiListing = None; $uiSelected = 0;
  $uiTable = None; $uiMetadata = None; $uiTimings = <||>;
  $uiStatus = "Connected. Enter a SAS library name and browse its files.";
  $uiTone = "Success";
);
uiDisconnected[] := (
  $uiListing = None; $uiSelected = 0; $uiTable = None; $uiMetadata = None;
  $uiBlocked = False; $uiStatus = "Local connection closed. Any successful import remains in kernel memory.";
  $uiTone = "Info";
);
uiReady[] := $session =!= None && !TrueQ[$uiBusy] && !TrueQ[$uiBlocked];
uiEntries[] := If[AssociationQ[$uiListing], Lookup[$uiListing, "Entries", {}], {}];
uiEntry[] := SelectFirst[uiEntries[], Lookup[#, "Index", 0] === $uiSelected &, None];
uiKind[] := If[AssociationQ[uiEntry[]], Lookup[uiEntry[], "Kind", ""], ""];
uiSupported[] := AssociationQ[uiEntry[]] && TrueQ[Lookup[uiEntry[], "SupportedImport", False]];
uiError[result_Failure] := (
  $uiTone = "Error";
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
PDSImportedData[] := If[AssociationQ[$uiImported], $uiImported,
  failure["NoUIImport", "No successful UI import is stored. Use Import into Wolfram first."]];
PDSClearImportedData[] := ($uiImported = None; $uiImportSummary = None; <|"Status" -> "Cleared"|>);

SetAttributes[uiRun, HoldAll];
uiRun[label_, body_] := Module[{result},
  If[TrueQ[$uiBusy], Return[Null]];
  $uiBusy = True; $uiTone = "Info"; $uiStatus = label;
  result = CheckAbort[
    Check[body,
      $uiBlocked = True; $uiTone = "Error";
      $uiStatus = "An unexpected Wolfram message interrupted this action. Disconnect and reconnect before retrying.";
    ],
    $uiBlocked = True; $uiTone = "Error";
    $uiStatus = "Evaluation aborted; operation outcome is uncertain. Disconnect, or use PDSReset[] if the session is unresponsive.";
  ];
  $uiBusy = False;
  Null
];


uiRelease[] := Module[{result},
  If[!AssociationQ[$uiTable], Return[True]];
  result = call["drop", {$uiTable["Token"]}];
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
uiBrowseRoot[] := Module[{uid, result},
  If[!uiReady[], Return[Null]];
  uid = StringTrim[$uiUID];
  If[uid === "", uiError[failure["EmptyCaslib", "Enter a SAS library name."]]; Return[Null]];
  uiRun["Browsing contribution...",
    If[uiRelease[],
      $uiListing = None; $uiSelected = 0; PDSClearImportedData[];
      result = call["browse", {uid, "", uid}];
      uiAcceptListing[result]
    ]
  ]
];
uiNavigate[direction_] := Module[{listing = $uiListing, entry = uiEntry[], result},
  If[!uiReady[] || !AssociationQ[listing], Return[Null]];
  If[direction === "Open" && (!AssociationQ[entry] || entry["Kind"] =!= "Directory"), Return[Null]];
  uiRun["Opening folder...",
    If[uiRelease[],
      $uiListing = None; $uiSelected = 0; PDSClearImportedData[];
      result = If[direction === "Open",
        call["select", {listing["ListingToken"], entry["Index"]}],
        call["browse", {listing["ContributionID"], listing["ParentDirectory"], listing["Caslib"]}]
      ];
      uiAcceptListing[result]
    ]
  ]
];
uiLoad[] := Module[{listing = $uiListing, entry = uiEntry[], result, metadata},
  If[!uiReady[] || !AssociationQ[listing] || !uiSupported[], Return[Null]];
  uiRun["Loading on SAS, then inspecting metadata...",
    If[uiRelease[],
      PDSClearImportedData[]; $uiMetadata = None;
      result = call["select", {listing["ListingToken"], entry["Index"]}];
      If[FailureQ[result], uiError[result],
        $uiTable = result; uiTime["Load", result];
        metadata = call["inspect", {result["Token"]}];
        If[FailureQ[metadata], uiError[metadata],
          $uiMetadata = metadata; uiTime["Inspect", metadata];
          uiNote["Table loaded on SAS. Metadata retrieved; patient records have not been imported.", result["LoadSeverity"]]
        ]
      ]
    ]
  ]
];
uiImport[] := Module[{result},
  If[!uiReady[] || !AssociationQ[$uiTable] || !AssociationQ[$uiMetadata], Return[Null]];
  If[!IntegerQ[$uiRowLimit] || $uiRowLimit < 1 || !IntegerQ[$uiCellLimit] || $uiCellLimit < 1,
    uiError[failure["InvalidLimit", "Row and cell limits must be positive whole numbers."]]; Return[Null]];
  uiRun["Importing into Wolfram memory...",
    PDSClearImportedData[];
    result = PDSFetchTable[$uiTable["Token"], "MaxRows" -> $uiRowLimit, "MaxCells" -> $uiCellLimit];
    If[FailureQ[result], uiError[result],
      $uiImported = result;
      $uiImportSummary = <|"Source" -> result["Provenance"]["RelativePath"],
        "Contribution" -> Lookup[result["Provenance"], "ContributionID", result["Provenance"]["Caslib"]],
        "Dimensions" -> result["Dimensions"], "Severity" -> result["Severity"]|>;
      uiTime["Import", result];
      uiNote["Import complete. Use data = PDSImportedData[]; in your notebook to access it.", result["Severity"]]
    ]
  ]
];
uiDrop[] := If[uiReady[], uiRun["Removing temporary table...",
  If[uiRelease[], uiNote["Temporary table removed. A successful local import, if any, remains available."]]
]];
uiDisconnect[] := Module[{result, notebook = $uiNotebook},
  If[TrueQ[$uiBusy], Return[Null]];
  uiRun["Disconnecting from SAS...",
    result = PDSDisconnect[];
    If[FailureQ[result], uiError[result],
      $uiNotebook = None;
      If[MatchQ[notebook, _NotebookObject], NotebookClose[notebook]]
    ]
  ]
];

uiFileChoices[] := If[uiEntries[] === {}, {0 -> "No entries available"},
  Prepend[(# ["Index"] -> (# ["Name"] <> "  [" <>
    If[# ["Kind"] === "Directory", "folder", If[TrueQ[# ["SupportedImport"]], "SAS table", "unsupported"]] <> "]")) & /@ uiEntries[],
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
  Row[{"SAS library name:  ", InputField[Dynamic[$uiUID], String, FieldSize -> 42, Enabled -> Dynamic[uiReady[]]]}],
  Button["Browse files", uiBrowseRoot[], Method -> "Queued", Enabled -> Dynamic[uiReady[]]],
  Style["2  Browse and select", 15, Bold],
  Dynamic[If[AssociationQ[$uiListing], Row[{"Browsing: ", $uiListing["ContributionID"], " / ",
    If[$uiListing["Directory"] === "", "(root)", $uiListing["Directory"]]}], "No folder listing."]],
  uiFileMenu[],
  Dynamic[If[AssociationQ[uiEntry[]], Row[{"Type: ", uiEntry[]["Kind"], "     Bytes: ", uiEntry[]["Bytes"],
    "     Path: ", uiEntry[]["RelativePath"]}], ""]],
  Row[{
    Button["Up one folder", uiNavigate["Up"], Method -> "Queued",
      Enabled -> Dynamic[uiReady[] && AssociationQ[$uiListing] && $uiListing["Directory"] =!= ""]], Spacer[10],
    Button["Open folder", uiNavigate["Open"], Method -> "Queued", Enabled -> Dynamic[uiReady[] && uiKind[] === "Directory"]], Spacer[10],
    Button["Load on SAS", uiLoad[], Method -> "Queued", Enabled -> Dynamic[uiReady[] && uiSupported[]]]
  }],
  uiHint["PDFs and ZIPs are listed but cannot be loaded as tables. Browsing or loading another source releases the current table."],
  Style["3  Table information", 15, Bold],
  Dynamic[uiTablePanel[]],
  Style["4  Optional import into Wolfram", 15, Bold],
  Row[{"Maximum rows  ", InputField[Dynamic[$uiRowLimit], Number, FieldSize -> 10, Enabled -> Dynamic[uiReady[]]],
    Spacer[15], "Maximum cells  ", InputField[Dynamic[$uiCellLimit], Number, FieldSize -> 12, Enabled -> Dynamic[uiReady[]]]}],
  uiHint["Limits apply to the complete table. Import transfers records to this Mac; no records are displayed here."],
  Row[{
    Button["Import into Wolfram", uiImport[], Method -> "Queued",
      Enabled -> Dynamic[uiReady[] && AssociationQ[$uiTable] && AssociationQ[$uiMetadata]]], Spacer[10],
    Button["Drop temporary table", uiDrop[], Method -> "Queued", Enabled -> Dynamic[uiReady[] && AssociationQ[$uiTable]]], Spacer[10],
    Button["Clear local import", PDSClearImportedData[], Method -> "Queued", Enabled -> Dynamic[!$uiBusy && AssociationQ[$uiImported]]]
  }],
  Dynamic[If[AssociationQ[$uiImportSummary], Row[{"Stored import: ", $uiImportSummary["Contribution"], " / ", $uiImportSummary["Source"],
      "   ", $uiImportSummary["Dimensions"], "   severity ", $uiImportSummary["Severity"]}], "No UI import stored."]],
  Dynamic[If[Length[$uiTimings] > 0, Row[{"Last completed timings (seconds): ",
    Row[KeyValueMap[Row[{#1, " ", #2}] &, $uiTimings], "   |   "]}], ""]],
  uiHint["Status: 0 = success; 1 = warning. Errors are shown above. Timings belong to the last completed action of each type."],
  Button["Disconnect and close", uiDisconnect[], Method -> "Queued", Enabled -> Dynamic[!$uiBusy]],
  uiHint["Use Disconnect and close to end the SAS session. Closing only the window leaves the connection open; reopen with PDSOpenBrowser[]."]
}, Alignment -> Left, Spacings -> .8, BaseStyle -> {FontFamily -> "Helvetica", FontSize -> 12}];

PDSOpenBrowser[] := Module[{},
  If[$session === None, Return[failure["NotConnected", "Run PDSConnect[] first."]]];
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
