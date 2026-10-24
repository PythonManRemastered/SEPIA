BeginPackage["SEPIARemote`"];
PDSConnect;
PDSListFiles;
PDSLoadTable;
PDSFetchTable;
PDSDropTable;
PDSDisconnect;
PDSReset;

PDSBrowseContribution;
PDSOpenEntry;
PDSInspectTable;

PDSOpenBrowser;
PDSImportedData;
PDSClearImportedData;

Begin["`Private`"];
$bridgePath = FileNameJoin[{DirectoryName[$InputFileName], "sepia_pds_bridge.py"}];
If[!ValueQ[$session], $session = None];

failure[code_, message_] := Failure[code, <|"MessageTemplate" -> message|>];

call[operation_String, arguments_List] := Module[{result},
  If[$session === None, Return[failure["NotConnected", "Run PDSConnect[] first."]]];
  result = Quiet[Check[
    ExternalEvaluate[$session,
      <|"Command" -> "bridge.dispatch", "Arguments" -> {operation, arguments}|>],
    $Failed
  ]];
  If[!AssociationQ[result], Return[failure["BridgeFailure",
    "No structured response. If the session is unresponsive, abort evaluation, run PDSReset[], then reconnect."]]];
  If[!TrueQ[Lookup[result, "OK", False]],
    Return[Failure[Lookup[result, "Code", "RemoteFailure"],
      <|"MessageTemplate" -> Lookup[result, "Message", "Remote operation failed."],
        "ElapsedSeconds" -> Lookup[result, "ElapsedSeconds", Missing["NotAvailable"]]|>]]];
  KeyDrop[result, "OK"]
];

connectInternal[python_: Automatic] := Module[{path, credentials, loaded, result},
  If[$session =!= None, Return[failure["SessionExists", "Disconnect or reset the existing connector session first."]]];
  path = If[python === Automatic,
    FileNameJoin[{$HomeDirectory, "Documents", "SEPIA", "sepia-python", "bin", "python"}], python];
  If[!StringQ[path], Return[failure["InvalidPython", "Supply a Python executable path."]]];
  If[!FileExistsQ[path] || !FileExistsQ[$bridgePath],
    Return[failure["MissingFile", "Python or sepia_pds_bridge.py is missing. Keep both package files together."]]];
  credentials = DialogInput[
    DynamicModule[{user = "", pass = ""}, Column[{
      Style["Connect to PDS", Bold],
      Grid[{
        {"SAS Username:", InputField[Dynamic[user], String]},
        {"SAS Password:", InputField[Dynamic[pass], String, FieldMasked -> True]}
      }, Alignment -> Left, Spacings -> {1, 1}],
      Row[{Button["Connect", DialogReturn[{StringTrim[user], pass}]], Spacer[15],
        Button["Cancel", DialogReturn[$Canceled]]}]
    }, Spacings -> 1]], WindowTitle -> "SEPIA Login"];
  If[credentials === $Canceled, Return[<|"Status" -> "Canceled"|>]];
  result = CheckAbort[
    Print["Starting SEPIA Python..."];
    $session = Quiet[Check[StartExternalSession[<|"System" -> "Python", "Evaluator" -> path|>], $Failed]];
    If[Head[$session] =!= ExternalSessionObject,
      $session = None;
      failure["PythonStartFailed", "Could not start the selected Python environment."],
      loaded = Quiet[Check[ExternalEvaluate[$session,
        <|"Command" -> "lambda path: exec(compile(open(path, encoding='utf-8').read(), path, 'exec'), globals())",
          "Arguments" -> {$bridgePath}|>], $Failed]];
      If[loaded === $Failed,
        PDSReset[];
        failure["BridgeLoadFailed", "Could not load the Python bridge."],
        Print["Connecting to SAS; waiting for server response..."];
        With[{answer = call["connect", credentials]},
          If[FailureQ[answer], PDSReset[]]; answer]
      ]
    ],
    Clear[credentials]; PDSReset[]; Abort[]
  ];
  Clear[credentials];
  result
];

Clear[PDSConnect];
Options[PDSConnect] = {"OpenUI" -> True};
finishConnect[python_, open_] := Module[{result, window},
  If[!MemberQ[{True, False}, open], Return[failure["InvalidOption", "OpenUI must be True or False."]]];
  result = connectInternal[python];
  If[AssociationQ[result] && Lookup[result, "Status", ""] === "Connected",
    uiNewSession[];
    If[TrueQ[open],
      window = Quiet[Check[PDSOpenBrowser[], $Failed]];
      If[window === $Failed || FailureQ[window],
        Return[Append[result, "UIStatus" -> "Could not open window. Connection is active; use PDSOpenBrowser[] or PDSDisconnect[]."]]]
    ]
  ];
  result
];
PDSConnect[OptionsPattern[]] := finishConnect[Automatic, OptionValue["OpenUI"]];
PDSConnect[python: (_String | Automatic), OptionsPattern[]] := finishConnect[python, OptionValue["OpenUI"]];
PDSConnect[python: Except[_String | Automatic | _Rule | _RuleDelayed], OptionsPattern[]] :=
  failure["InvalidPython", "Supply a Python executable path."];

PDSListFiles[caslib_String, directory_String: ""] := (
  Print["Listing remote files..."]; call["files", {caslib, directory}]);
PDSLoadTable[caslib_String, path_String] := (
  Print["Loading a temporary SAS table..."]; call["load", {caslib, path}]);

Options[PDSBrowseContribution] = {"Caslib" -> Automatic};
PDSBrowseContribution[uid_String, directory_String: "", OptionsPattern[]] := Module[{caslib},
  caslib = Replace[OptionValue["Caslib"], Automatic -> uid];
  If[!StringQ[caslib], Return[failure["InvalidCaslib", "Supply an exact caslib name."]]];
  Print["Browsing contribution files..."];
  call["browse", {uid, directory, caslib}]
];
PDSOpenEntry[listing_Association, index_Integer] := Module[{token},
  token = Lookup[listing, "ListingToken", Missing["NotAvailable"]];
  If[!StringQ[token], Return[failure["InvalidListing", "Use a successful PDSBrowseContribution result."]]];
  Print["Opening selected entry..."]; call["select", {token, index}]
];
PDSInspectTable[token_String] := (Print["Retrieving table metadata..."]; call["inspect", {token}]);

Options[PDSFetchTable] = {"MaxRows" -> 1000, "MaxCells" -> 200000};
PDSFetchTable[token_String, OptionsPattern[]] := Module[{rows, cells, answer},
  rows = OptionValue["MaxRows"]; cells = OptionValue["MaxCells"];
  If[!IntegerQ[rows] || !IntegerQ[cells] || rows < 1 || cells < 1,
    Return[failure["InvalidLimit", "MaxRows and MaxCells must be positive integers."]]];
  Print["Fetching table into Mac memory; waiting for server response..."];
  answer = call["fetch", {token, rows, cells}];
  If[FailureQ[answer], Return[answer]];
  answer["Data"] = Map[
    If[AssociationQ[#] && Lookup[#, "SEPIAType", ""] === "Missing", Missing["CASMissing"], #] &,
    answer["Data"], {2}];
  answer
];
PDSDropTable[token_String] := (Print["Removing temporary table..."]; call["drop", {token}]);

PDSDisconnect[] := Module[{answer},
  If[$session === None, Return[<|"Status" -> "AlreadyClosed"|>]];
  Print["Closing the SAS session..."];
  answer = call["close", {}];
  If[!FailureQ[answer], PDSReset[]];
  answer
];
PDSReset[] := Module[{},
  If[$session =!= None, Quiet[Check[DeleteObject[$session], Null]]];
  $session = None;
  uiDisconnected[];
  <|"Status" -> "LocalProcessStopped", "ServerCleanup" -> "NotConfirmed"|>
];

Get[FileNameJoin[{DirectoryName[$InputFileName], "SEPIABrowser.wl"}]];

End[];
EndPackage[];
