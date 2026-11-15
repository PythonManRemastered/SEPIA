BeginPackage["SEPIAZIPLoadCheck`"];
PDSZIPLoadCheck;
Begin["`Private`"];
$member = "adsl.sas7bdat";
$memberBytes = 2359296;
$memberSHA256 = "72f05e5230370b7462b7afc2d0f022aadf7587fbd864adf2fab94e053df33cc3";
fail[tag_, text_] := Failure[tag, <|"MessageTemplate" -> text|>];
little[bytes_List, offset_Integer, count_Integer] := FromDigits[Reverse[Take[bytes, {offset + 1, offset + count}]], 256];
readAt[stream_, position_, count_] := (SetStreamPosition[stream, position]; BinaryReadList[stream, "Byte", count]);
memberDirectory[path_] := Module[{stream, result, stop, length, tail, positions, eocd, endPosition,
    count, offset, size, cursor, header, nameLength, extraLength, commentLength, name, chosen = None,
    local, localName, kind, i},
  stream = Quiet[Check[OpenRead[path, BinaryFormat -> True], $Failed]];
  If[!MatchQ[stream, _InputStream], Return[fail["ArchiveReadFailed", "Could not open the temporary ZIP."]]];
  result = WithCleanup[Null, Quiet[Check[Catch[
    length = FileByteCount[path];
    tail = readAt[stream, Max[0, length - 65557], Min[length, 65557]];
    positions = Select[Reverse[First /@ SequencePosition[tail, {80,75,5,6}]],
      # + 21 <= Length[tail] && # + 21 + little[tail, # + 19, 2] == Length[tail] &];
    If[Length[positions] =!= 1, Throw[$Failed, stop]];
    endPosition = Max[0, length - 65557] + First[positions] - 1;
    eocd = Take[tail, {First[positions], First[positions] + 21}];
    count = little[eocd, 10, 2]; offset = little[eocd, 16, 4]; size = little[eocd, 12, 4];
    If[little[eocd, 4, 2] =!= 0 || little[eocd, 6, 2] =!= 0 || count == 65535 ||
      count =!= little[eocd, 8, 2] || offset + size =!= endPosition, Throw[$Failed, stop]];
    cursor = offset;
    Do[
      If[cursor + 46 > offset + size, Throw[$Failed, stop]];
      header = readAt[stream, cursor, 46];
      If[Length[header] =!= 46 || Take[header,4] =!= {80,75,1,2}, Throw[$Failed, stop]];
      nameLength = little[header,28,2]; extraLength = little[header,30,2]; commentLength = little[header,32,2];
      If[cursor + 46 + nameLength + extraLength + commentLength > offset + size || little[header,34,2] =!= 0,
        Throw[$Failed, stop]];
      name = readAt[stream, cursor + 46, nameLength];
      If[!AllTrue[name, 32 <= # <= 126 &], Throw[$Failed, stop]];
      name = FromCharacterCode[name];
      If[name === $member,
        If[chosen =!= None, Throw[$Failed, stop]];
        kind = BitAnd[BitShiftRight[little[header,38,4],16],61440];
        If[!MemberQ[{0,32768}, kind] || BitAnd[little[header,38,4],16] =!= 0 ||
          BitAnd[little[header,8,2],1] =!= 0 || !MemberQ[{0,8},little[header,10,2]] ||
          little[header,24,4] =!= $memberBytes, Throw[$Failed, stop]];
        chosen = <|"CompressedBytes" -> little[header,20,4], "Bytes" -> little[header,24,4],
          "LocalOffset" -> little[header,42,4], "Flags" -> little[header,8,2], "Method" -> little[header,10,2]|>;
      ];
      cursor += 46 + nameLength + extraLength + commentLength,
      {i,count}];
    If[cursor =!= offset + size || !AssociationQ[chosen], Throw[$Failed, stop]];
    local = readAt[stream,chosen["LocalOffset"],30];
    If[Length[local] =!= 30 || Take[local,4] =!= {80,75,3,4} ||
      little[local,6,2] =!= chosen["Flags"] || little[local,8,2] =!= chosen["Method"] ||
      chosen["LocalOffset"] + 30 + little[local,26,2] + little[local,28,2] + chosen["CompressedBytes"] > offset,
      Throw[$Failed,stop]];
    localName = readAt[stream,chosen["LocalOffset"] + 30,little[local,26,2]];
    If[localName =!= ToCharacterCode[$member],Throw[$Failed,stop]];
    chosen,
    stop], $Failed]], Quiet[Close[stream]]];
  If[!AssociationQ[result], fail["UnsupportedOrUnsafeZIPMember",
    "The selected ZIP member failed directory validation. This check requires an ordinary, unencrypted, regular adsl.sas7bdat of the previously verified size. Nothing was extracted."], result]
];
extractMember[zip_, names_List, directory_String] := Module[{entry, extracted, file, hash, files},
  If[Count[names,$member] =!= 1 || FailureQ[SEPIAZIPProbe`Private`checkNames[names]],
    Return[fail["InvalidMemberSelection", "The archive does not contain one unambiguous adsl.sas7bdat member."]]];
  entry = memberDirectory[zip];
  If[FailureQ[entry], Return[entry]];
  extracted = Quiet[Check[ExtractArchive[zip, directory, $member, OverwriteTarget -> False], $Failed]];
  file = FileNameJoin[{directory,$member}];
  files = FileNames[All,directory,Infinity];
  If[!ListQ[extracted] || files =!= {file} || FileType[file] =!= File || FileByteCount[file] =!= $memberBytes,
    Return[fail["ExtractionMismatch", "Extraction did not produce exactly the selected SAS file with the expected size. No upload was attempted."]]];
  hash = Quiet[Check[IntegerString[FileHash[file,"SHA256"],16,64],$Failed]];
  If[hash =!= $memberSHA256, Return[fail["MemberHashMismatch",
    "The extracted file differs from the SHA-256 recorded for your earlier local import. No upload was attempted."]]];
  <|"File" -> file, "Bytes" -> $memberBytes, "SHA256" -> hash|>
];
deleteDirectory[d_String] := If[DirectoryQ[d], Quiet[Check[DeleteDirectory[d,DeleteContents -> True]; !DirectoryQ[d],False]],True];
deleteDirectory[_] := True;
uploadCall[bytes_,source_] := SEPIANative`PDSNativeUploadSAS[bytes,source];
inspectCall[token_] := SEPIANative`PDSNativeInspectTable[token];
dropCall[token_] := SEPIANative`PDSNativeDropTable[token];
processMember[zip_,names_] := Module[{directory = None, extracted, bytes, uploaded = None, inspected,
    result, stop, deleted = True, dropped = <|"Status" -> "NotCreated"|>},
  result = WithCleanup[Null,Catch[
    directory = Quiet[Check[CreateDirectory[], $Failed]];
    If[!StringQ[directory],Throw[fail["TemporaryDirectoryFailed","Could not create a temporary extraction directory."],stop]];
    Print["Extracting only adsl.sas7bdat and checking its SHA-256..."];
    extracted = extractMember[zip,names,directory];
    If[FailureQ[extracted],Throw[extracted,stop]];
    bytes = Quiet[Check[ByteArray[BinaryReadList[extracted["File"],"Byte"]],$Failed]];
    If[!MatchQ[bytes,_ByteArray],Throw[fail["MemberReadFailed","Could not read the selected SAS file for upload."],stop]];
    Print["Uploading the original SAS bytes to a temporary CAS table..."];
    uploaded = uploadCall[bytes,<|"ContributionID" -> "Breast_EliLill_2008_168",
      "SourceArchive" -> "adam20161103.zip", "ArchiveMember" -> $member|>];
    bytes = None;
    If[FailureQ[uploaded],Throw[uploaded,stop]];
    If[!AssociationQ[uploaded] || !StringQ[Lookup[uploaded,"Token",None]],
      Throw[fail["UploadResultInvalid","SAS did not return a tracked temporary-table token."],stop]];
    inspected = inspectCall[uploaded["Token"]];
    If[FailureQ[inspected],Throw[inspected,stop]];
    If[!AssociationQ[inspected] || Lookup[inspected,"Dimensions",None] =!= {385,110},
      Throw[fail["DimensionsMismatch","The uploaded archive member did not have the expected 385 rows and 110 columns."],stop]];
    <|"Status" -> "ZIPMemberInspected", "SelectedMember" -> $member, "MembersExtracted" -> 1,
      "ExtractedBytes" -> extracted["Bytes"], "SHA256MatchesEarlierLocalFile" -> True,
      "Dimensions" -> inspected["Dimensions"], "ColumnNames" -> inspected["ColumnNames"],
      "MatchesExpectedDimensions" -> True, "LoadSeverity" -> uploaded["LoadSeverity"], "InspectionSeverity" -> inspected["Severity"]|>,
    stop],
    bytes = None;
    If[AssociationQ[uploaded] && StringQ[Lookup[uploaded,"Token",None]], dropped = dropCall[uploaded["Token"]]];
    deleted = deleteDirectory[directory];
  ];
  extracted = <|"ExtractedDirectoryDeleted" -> deleted, "TableCleanup" -> dropped|>;
  If[!TrueQ[deleted], extracted["TemporaryDirectory"] = directory];
  If[FailureQ[result],Failure[result[[1]],Join[result[[2]],extracted]],Join[result,extracted]]
];
connectCall[] := SEPIANative`PDSNativeConnect[];
disconnectCall[] := SEPIANative`PDSNativeDisconnect[];
PDSZIPLoadCheck[] := Module[{connected, result, closed, ownSession = False, started = AbsoluteTime[]},
  If[SEPIANative`Private`$connection =!= None,
    Return[fail["ExistingSASSession","Disconnect your current SEPIA SAS session before running this standalone check. It will create and close its own test session."]]];
  connected = connectCall[];
  If[FailureQ[connected] || !AssociationQ[connected] || Lookup[connected,"Status",""] =!= "Connected",Return[connected]];
  ownSession = True;
  result = WithCleanup[Null,
    SEPIAZIPProbe`Private`runCheck[SEPIAZIPProbe`Private`credentials[],processMember],
    closed = If[ownSession,disconnectCall[],<|"Status" -> "NotStarted"|>]
  ];
  <|"Connection" -> KeyTake[connected,{"Status","AccessMethod","ConnectorVersion"}],
    "ZIP" -> If[FailureQ[result],Join[<|"FailureTag" -> result[[1]]|>,result[[2]]],result],
    "SASDisconnect" -> closed, "ElapsedSeconds" -> Round[AbsoluteTime[] - started,.001]|>
];
End[];
EndPackage[];
