BeginPackage["SEPIANative`"];
PDSNativeUploadSAS;
Begin["`Private`"];
uploadSAS[bytes_ByteArray, source_Association, timeout_] := Module[
  {token, name, record, parameters, answer, payload, disposition, severity},
  If[!validTimeout[timeout], Return[failure["InvalidTimeout", "Timeout must be positive and at most 300 seconds."]]];
  If[$connection === None, Return[failure["NotConnected", "Connect to SAS before uploading a ZIP member."]]];
  If[!AllTrue[Lookup[source, {"ContributionID", "SourceArchive", "ArchiveMember"}, None], StringQ[#] && # =!= "" &] ||
    !validPath[source["ArchiveMember"]] || !StringEndsQ[ToLowerCase[source["ArchiveMember"]], ".sas7bdat"],
    Return[failure["InvalidUploadSource", "Supply a contribution, source ZIP and relative SAS7BDAT member name."]]];
  If[Length[bytes] < 32 || Take[Normal[bytes], 32] =!=
      {0,0,0,0,0,0,0,0,0,0,0,0,194,234,129,96,179,20,17,207,189,146,8,0,9,199,49,140,24,31,16,17},
    Return[failure["NotSAS7BDAT", "The selected member does not have the SAS7BDAT file signature."]]];
  If[AnyTrue[Values[$tables], Lookup[#, "State", ""] =!= "Loaded" &],
    Return[failure["CleanupRequired", "An earlier table operation is unresolved. Disconnect before another upload."]]];
  token = CreateUUID[];
  name = "sepia_n_" <> StringTake[StringReplace[token, "-" -> ""], 24];
  record = <|"Token" -> token, "Caslib" -> source["ContributionID"],
    "RelativePath" -> source["SourceArchive"] <> "!" <> source["ArchiveMember"],
    "TemporaryTable" -> name, "TemporaryCaslib" -> "CASUSER", "State" -> "LoadUnconfirmed",
    "SourceKind" -> "ZIPMemberUpload", "SourceArchive" -> source["SourceArchive"],
    "ArchiveMember" -> source["ArchiveMember"]|>;
  AssociateTo[$tables, token -> record];
  parameters = <|"importOptions" -> <|"fileType" -> "basesas"|>,
    "casout" -> <|"name" -> name, "caslib" -> "CASUSER", "replace" -> False, "promote" -> False|>|>;
  answer = request["PUT", "cas/sessions/" <> $connection["Session"] <> "/actions/table.upload", bytes,
    $connection["Authorization"], $connection["Affinity"], timeout,
    <|"Content-Type" -> "application/octet-stream", "JSON-Parameters" -> ExportString[parameters, "RawJSON", "Compact" -> True]|>];
  If[FailureQ[answer], Return[loadFailure[answer, record]]];
  $connection["Affinity"] = answer["Affinity"];
  payload = answer["Payload"];
  disposition = Lookup[payload, "disposition", <||>];
  severity = If[AssociationQ[disposition], Lookup[<|"Normal" -> 0, "Warning" -> 1, "Error" -> 2|>,
    Lookup[disposition, "severity", ""], None], None];
  If[!IntegerQ[severity] || severity >= 2,
    Return[loadFailure[failure["UploadUnconfirmed", "SAS did not confirm a successful table upload. End the SAS session before retrying."], record]]];
  record = Join[record, <|"State" -> "Loaded", "LoadSeverity" -> severity|>];
  AssociateTo[$tables, token -> record];
  Join[KeyDrop[record, "State"], <|"Status" -> "Loaded", "LoadStatus" -> If[severity == 0, "Success", "Warning"]|>]
];
Options[PDSNativeUploadSAS] = {"Timeout" -> 120};
PDSNativeUploadSAS[bytes_ByteArray, source_Association, OptionsPattern[]] :=
  run[uploadSAS[bytes, source, OptionValue["Timeout"]]];
End[];
EndPackage[];
