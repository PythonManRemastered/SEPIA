BeginPackage["SEPIANative`"];
PDSNativeOpenZIP;
PDSNativeLoadZIPMember;
PDSNativeCloseZIP;
Begin["`Private`"];
If[!ValueQ[$zipWebsite],$zipWebsite=None];
If[!ValueQ[$zipArchives],$zipArchives=<||>];
If[!ValueQ[$zipPendingDirectories],$zipPendingDirectories=<||>];
If[!ValueQ[$zipCleanupBlocked],$zipCleanupBlocked=False];
zipRequest[method_,route_,body_:""] := Module[{answer},
  If[!AssociationQ[$zipWebsite],Return[failure["WebsiteNotConnected","Connect to the PDS website to open a ZIP."]]];
  answer = SEPIAZIPProbe`Private`request[method,route,body,$zipWebsite["Cookies"]];
  If[FailureQ[answer],Return[answer]];
  $zipWebsite["Cookies"] = SEPIAZIPProbe`Private`mergeCookies[$zipWebsite["Cookies"],answer];
  answer
];
zipJSON[answer_] := If[AssociationQ[answer],SEPIAZIPProbe`Private`json[Lookup[answer,"BodyByteArray",None]],$Failed];
zipEnsureWebsite[] := Module[{answer,data,credentials,uuid,stop,result},
  If[AssociationQ[$zipWebsite] && StringQ[Lookup[$zipWebsite,"UUID",None]],
    answer=zipRequest["GET","/api/public/user/session"];
    If[FailureQ[answer],Return[answer]];
    data=zipJSON[answer];
    If[AssociationQ[data] && Lookup[data,"uuid",None]===$zipWebsite["UUID"] && TrueQ[Lookup[data,"attested",False]],Return[True]];
    $zipWebsite=None
  ];
  credentials=SEPIAZIPProbe`Private`credentials[];
  If[credentials===$Canceled,Return[<|"Status"->"Canceled"|>]];
  If[FailureQ[credentials],Return[credentials]];
  $zipWebsite=<|"Cookies"->{}|>;
  result=WithCleanup[Null,Catch[
    answer=zipRequest["GET","/api/public/user/session"];
    If[FailureQ[answer],Throw[answer,stop]];
    answer=zipRequest["POST","/api/auth/login",ExportString[<|"username"->credentials[[1]],"password"->credentials[[2]]|>,"RawJSON"]];
    credentials=None;
    If[FailureQ[answer],Throw[answer,stop]];
    data=zipJSON[answer];
    uuid=If[AssociationQ[data],Lookup[data,"uuid",None],None];
    If[!StringQ[uuid] || uuid==="",Throw[failure["WebsiteLoginUnconfirmed","PDS website login was not confirmed."],stop]];
    $zipWebsite["UUID"]=uuid;
    answer=zipRequest["GET","/api/public/user/session"];
    If[FailureQ[answer],Throw[answer,stop]];
    data=zipJSON[answer];
    If[!AssociationQ[data] || Lookup[data,"uuid",None]=!=uuid,
      Throw[failure["WebsiteSessionNotRetained","The website session was not retained after login."],stop]];
    If[!TrueQ[Lookup[data,"attested",False]],Throw[failure["WebsiteAttestationRequired","Complete the required attestation on the PDS website before opening ZIPs."],stop]];
    True,stop],credentials=None];
  result
];
zipResolve[uid_String,filename_String,bytes_Integer] := Module[{parameters,answer,catalogue,matches,study,groups,files,file},
  parameters=<|"searchText"->"","searchSort"->"Most Recent","access"->{},"arm"->{},"dataProvider"->{},
    "sponsor"->{},"studyPhase"->{},"tumorType"->{},"linkedData"->{}|>;
  answer=zipRequest["POST","/api/accessdata/results",ExportString[parameters,"RawJSON"]];
  If[FailureQ[answer],Return[answer]];
  catalogue=zipJSON[answer];
  If[!ListQ[catalogue],Return[failure["InvalidStudyCatalogue","PDS returned no recognized study catalogue."]]];
  matches=Select[catalogue,AssociationQ[#] && Lookup[#,"uniqueDatasetId",None]===uid &];
  If[Length[matches]=!=1 || !validSession[Lookup[First[matches],"uuid",None]],
    Return[failure["ContributionNotUnique","The exact SAS library name did not identify one PDS website contribution. No study identifier was guessed."]]];
  answer=zipRequest["GET","/api/accessdata/results/"<>First[matches]["uuid"]];
  If[FailureQ[answer],Return[answer]];
  study=zipJSON[answer];
  If[!AssociationQ[study] || Lookup[study,"uniqueDatasetId",None]=!=uid ||
      Lookup[study,"uuid",None]=!=First[matches]["uuid"] || !IntegerQ[Lookup[study,"donationId",None]],
    Return[failure["ContributionMismatch","The contribution metadata did not match the selected SAS library."]]];
  groups=Lookup[study,"fileGroups",None];
  If[!ListQ[groups] || !AllTrue[groups,AssociationQ],Return[failure["InvalidWebsiteFiles","PDS returned no recognized file groups."]]];
  files=Cases[groups,a_Association /; Lookup[a,"filename",None]===filename,Infinity];
  If[Length[files]=!=1,Return[failure["ArchiveNotUnique","The selected ZIP filename was not uniquely identified in this contribution. No archive was guessed."]]];
  file=First[files];
  If[!TrueQ[Lookup[file,"downloadable",False]],Return[failure["DownloadNotAuthorized","PDS does not authorize downloading this ZIP in the current website session."]]];
  If[!IntegerQ[Lookup[file,"id",None]] || file["id"]<=0 || Lookup[file,"donationId",None]=!=study["donationId"] ||
      Lookup[file,"fileSize",None]=!=bytes || bytes<22,
    Return[failure["ArchiveMetadataMismatch","The website ZIP identifier or size does not match the SAS file listing. No download was attempted."]]];
  <|"UUID"->study["uuid"],"DonationID"->study["donationId"],"FileID"->file["id"],"Bytes"->bytes|>
];
zipLittle[b_List,o_Integer,n_Integer] := FromDigits[Reverse[Take[b,{o+1,o+n}]],256];
zipReadAt[s_,p_,n_] := (SetStreamPosition[s,p];BinaryReadList[s,"Byte",n]);
zipSafeName[name_String] := SEPIAZIPProbe`Private`safeMember[name] &&
  AllTrue[StringSplit[StringTrim[name,"/"],"/",All],#=!="" && #=!="." && #=!=".." &];
zipSafeName[_] := False;
zipDirectory[path_String] := Module[{stream,result,stop,length,tail,positions,endPosition,eocd,count,offset,size,cursor,
    header,n,e,c,raw,name,entries={},flags,method,kind,bytes,local,i},
  stream=Quiet[Check[OpenRead[path,BinaryFormat->True],$Failed]];
  If[!MatchQ[stream,_InputStream],Return[failure["ArchiveReadFailed","Could not open the temporary ZIP."]]];
  result=WithCleanup[Null,Quiet[Check[Catch[
    length=FileByteCount[path];
    tail=zipReadAt[stream,Max[0,length-65557],Min[length,65557]];
    positions=Select[Reverse[First /@ SequencePosition[tail,{80,75,5,6}]],
      #+21<=Length[tail] && #+21+zipLittle[tail,#+19,2]==Length[tail] &];
    If[Length[positions]=!=1,Throw[$Failed,stop]];
    endPosition=Max[0,length-65557]+First[positions]-1;
    eocd=Take[tail,{First[positions],First[positions]+21}];
    count=zipLittle[eocd,10,2];offset=zipLittle[eocd,16,4];size=zipLittle[eocd,12,4];
    If[zipLittle[eocd,4,2]=!=0 || zipLittle[eocd,6,2]=!=0 || count==65535 ||
      count=!=zipLittle[eocd,8,2] || offset+size=!=endPosition,Throw[$Failed,stop]];
    cursor=offset;
    Do[
      If[cursor+46>offset+size,Throw[$Failed,stop]];
      header=zipReadAt[stream,cursor,46];
      If[Length[header]=!=46 || Take[header,4]=!={80,75,1,2},Throw[$Failed,stop]];
      n=zipLittle[header,28,2];e=zipLittle[header,30,2];c=zipLittle[header,32,2];flags=zipLittle[header,8,2];
      method=zipLittle[header,10,2];kind=BitAnd[BitShiftRight[zipLittle[header,38,4],16],61440];
      bytes=zipLittle[header,24,4];local=zipLittle[header,42,4];
      If[cursor+46+n+e+c>offset+size || zipLittle[header,34,2]=!=0 ||
        MemberQ[{bytes,zipLittle[header,20,4],local},4294967295],Throw[$Failed,stop]];
      raw=zipReadAt[stream,cursor+46,n];
      If[Length[raw]=!=n || (!AllTrue[raw,#<128 &] && BitAnd[flags,2048]==0),Throw[$Failed,stop]];
      name=FromCharacterCode[raw,"UTF-8"];
      If[!zipSafeName[name],Throw[$Failed,stop]];
      AppendTo[entries,<|"Index"->Length[entries]+1,"Name"->name,"RelativePath"->name,"Bytes"->bytes,
        "CompressedBytes"->zipLittle[header,20,4],"LocalOffset"->local,"CentralOffset"->offset,
        "Flags"->flags,"Method"->method,"CRC32"->zipLittle[header,16,4],
        "Kind"->If[StringEndsQ[name,"/"],"Directory",If[MemberQ[{0,32768},kind] && BitAnd[zipLittle[header,38,4],16]==0,"File","Unsupported"]],
        "SupportedImport"->(!StringEndsQ[name,"/"] && MemberQ[{0,32768},kind] &&
          BitAnd[zipLittle[header,38,4],16]==0 && BitAnd[flags,1]==0 && MemberQ[{0,8},method] &&
          !StringContainsQ[name,{"*","?","[","]","@"}] && StringEndsQ[ToLowerCase[name],".sas7bdat"]),
        "Permissions"->""|>];
      cursor+=46+n+e+c,
      {i,count}];
    If[cursor=!=offset+size || !DuplicateFreeQ[ToLowerCase /@ Lookup[entries,"Name"]],Throw[$Failed,stop]];
    entries,stop],$Failed]],Quiet[Close[stream]]];
  If[!ListQ[result],failure["UnsupportedOrUnsafeZIP","The ZIP directory is unsafe or uses an unsupported archive layout. ZIP64, split archives and legacy non-ASCII names are not supported."],result]
];
zipConfirmTransfer[bytes_,label_] := If[bytes<=100 1024^2,True,
  If[$FrontEnd===Null,False,TrueQ[DialogInput[Column[{Style["Transfer a large file?",16,Bold],
    Pane[label,{450,Automatic}],Row[{"Bytes: ",bytes}],
    "This transfer uses temporary local storage and memory.",
    Row[{Button["Cancel",DialogReturn[False]],Spacer[15],Button["Continue",DialogReturn[True]]}]}],WindowTitle->"SEPIA File Transfer"]]]];
zipDeleteDirectory[d_String] := If[DirectoryQ[d],Quiet[Check[DeleteDirectory[d,DeleteContents->True];!DirectoryQ[d],False]],True];
zipDeleteTracked[d_String] := If[zipDeleteDirectory[d],KeyDropFrom[$zipPendingDirectories,d];True,
  $zipCleanupBlocked=True;False];
zipPublicListing[record_] := <|"ListingKind"->"ZIP","ArchiveToken"->record["Token"],"ContributionID"->record["ContributionID"],
  "Caslib"->record["ContributionID"],"Directory"->record["SourceArchive"],"ParentDirectory"->"",
  "Entries"->(KeyTake[#, {"Index","Name","RelativePath","Bytes","Kind","SupportedImport","Permissions"}]& /@ record["Entries"]),
  "Severity"->0|>;
zipOpen[supplied_Association,index_Integer] := Module[{token,listing,entry,access,metadata,answer,bytes,directory=None,path,
    entries,record,result,stop,owned=False,deleted=True},
  If[$connection===None,Return[failure["NotConnected","Connect to SAS before opening a ZIP."]]];
  If[TrueQ[$zipCleanupBlocked],Return[failure["CleanupRequired","Disconnect to complete temporary-file cleanup before opening another ZIP."]]];
  token=Lookup[supplied,"ListingToken",None];
  If[!StringQ[token] || !KeyExistsQ[$listings,token],Return[failure["UnknownListing","Browse again in the current SAS connection."]]];
  listing=$listings[token];
  If[index<1 || index>Length[listing["Entries"]],Return[failure["InvalidSelection","Choose a ZIP from the current listing."]]];
  entry=listing["Entries"][[index]];
  If[entry["Kind"]=!="File" || !StringEndsQ[ToLowerCase[entry["Name"]],".zip"],Return[failure["NotZIP","Select a regular ZIP file."]]];
  If[!TrueQ[zipConfirmTransfer[entry["Bytes"],entry["RelativePath"]]],Return[<|"Status"->"Canceled"|>]];
  result=WithCleanup[Null,Catch[
    access=zipEnsureWebsite[];
    If[FailureQ[access] || AssociationQ[access],Throw[access,stop]];
    metadata=zipResolve[listing["ContributionID"],FileNameTake[entry["Name"]],entry["Bytes"]];
    If[FailureQ[metadata],Throw[metadata,stop]];
    answer=zipRequest["GET","/api/file/download/"<>ToString[metadata["DonationID"]]<>"/"<>ToString[metadata["FileID"]]];
    If[FailureQ[answer],Throw[answer,stop]];
    bytes=Lookup[answer,"BodyByteArray",None];
    If[!MatchQ[bytes,_ByteArray] || Length[bytes]=!=metadata["Bytes"],Throw[failure["ArchiveLengthMismatch","The ZIP download did not match its declared size."],stop]];
    directory=Quiet[Check[CreateDirectory[],$Failed]];
    If[!StringQ[directory],Throw[failure["TemporaryDirectoryFailed","Could not create temporary ZIP storage."],stop]];
    AssociateTo[$zipPendingDirectories,directory->True];
    path=FileNameJoin[{directory,"source.zip"}];
    answer=SEPIAZIPProbe`Private`writeBytes[path,bytes];bytes=None;
    If[FailureQ[answer],Throw[answer,stop]];
    entries=zipDirectory[path];
    If[FailureQ[entries],Throw[entries,stop]];
    token=CreateUUID[];
    record=<|"Token"->token,"ContributionID"->listing["ContributionID"],"SourceArchive"->entry["RelativePath"],
      "Directory"->directory,"Path"->path,"Entries"->entries,"SHA256"->IntegerString[FileHash[path,"SHA256"],16,64],
      "WebsiteSource"->KeyTake[metadata,{"UUID","DonationID","FileID"}]|>;
    AssociateTo[$zipArchives,token->record];owned=True;
    zipPublicListing[record],stop],
    bytes=None;
    If[!owned && StringQ[directory],deleted=zipDeleteTracked[directory]]
  ];
  If[!deleted,Return[failure["ZIPCleanupFailed","Temporary ZIP storage could not be removed. Disconnect before continuing.",<|"TemporaryDirectory"->directory|>]]];
  result
];
PDSNativeOpenZIP[listing_Association,index_Integer] := run[zipOpen[listing,index]];
zipRecord[token_String] := If[$connection===None,failure["NotConnected","Connect to SAS first."],
  Lookup[$zipArchives,token,failure["UnknownArchive","Open the ZIP again in the current browser session."]]];
zipExtract[archive_,entry_,directory_] := Module[{stream,header,raw,valid,created,file,files},
  stream=Quiet[Check[OpenRead[archive["Path"],BinaryFormat->True],$Failed]];
  If[!MatchQ[stream,_InputStream],Return[failure["ArchiveReadFailed","Could not read the temporary ZIP."]]];
  valid=WithCleanup[Null,Quiet[Check[
    header=zipReadAt[stream,entry["LocalOffset"],30];
    If[Length[header]=!=30,False,
      raw=zipReadAt[stream,entry["LocalOffset"]+30,zipLittle[header,26,2]];
      Take[header,4]==={80,75,3,4} && zipLittle[header,6,2]===entry["Flags"] && zipLittle[header,8,2]===entry["Method"] &&
      raw===ToCharacterCode[entry["Name"],"UTF-8"] && entry["LocalOffset"]+30+zipLittle[header,26,2]+zipLittle[header,28,2]+entry["CompressedBytes"]<=entry["CentralOffset"]
    ],False]],Quiet[Close[stream]]];
  If[!TrueQ[valid],Return[failure["ZIPMemberMismatch","The selected member did not match its validated ZIP directory entry."]]];
  created=Quiet[Check[ExtractArchive[archive["Path"],directory,entry["Name"],OverwriteTarget->False],$Failed]];
  file=FileNameJoin[Prepend[StringSplit[entry["Name"],"/"],directory]];
  files=Select[FileNames[All,directory,Infinity],FileType[#]===File &];
  If[!ListQ[created] || files=!={file} || FileType[file]=!=File || FileByteCount[file]=!=entry["Bytes"],
    Return[failure["ExtractionMismatch","Extraction did not produce exactly the selected SAS file with its declared size. No upload was attempted."]]];
  <|"File"->file,"SHA256"->IntegerString[FileHash[file,"SHA256"],16,64]|>
];
zipLoadMember[token_String,index_Integer] := Module[{archive,entry,directory=None,extracted,bytes,result,stop,deleted=True,source},
  If[TrueQ[$zipCleanupBlocked],Return[failure["CleanupRequired","Disconnect to complete temporary-file cleanup before loading another member."]]];
  archive=zipRecord[token];If[FailureQ[archive],Return[archive]];
  If[index<1 || index>Length[archive["Entries"]],Return[failure["InvalidSelection","Choose a member from the open ZIP."]]];
  If[!TrueQ[Quiet[Check[FileExistsQ[archive["Path"]] &&
      IntegerString[FileHash[archive["Path"],"SHA256"],16,64]===archive["SHA256"],False]]],
    Return[failure["ArchiveChanged","The temporary ZIP is missing or differs from the downloaded archive. Close and reopen it before loading a member."]]];
  entry=archive["Entries"][[index]];
  If[!TrueQ[entry["SupportedImport"]],Return[failure["UnsupportedZIPMember","Only unencrypted regular SAS7BDAT members using supported ZIP compression can be loaded."]]];
  If[!TrueQ[zipConfirmTransfer[entry["Bytes"],entry["Name"]]],Return[<|"Status"->"Canceled"|>]];
  result=WithCleanup[Null,Catch[
    directory=Quiet[Check[CreateDirectory[],$Failed]];
    If[!StringQ[directory],Throw[failure["TemporaryDirectoryFailed","Could not create temporary extraction storage."],stop]];
    AssociateTo[$zipPendingDirectories,directory->True];
    extracted=zipExtract[archive,entry,directory];If[FailureQ[extracted],Throw[extracted,stop]];
    bytes=Quiet[Check[ByteArray[BinaryReadList[extracted["File"],"Byte"]],$Failed]];
    If[!MatchQ[bytes,_ByteArray],Throw[failure["MemberReadFailed","Could not read the selected member for upload."],stop]];
    source=<|"ContributionID"->archive["ContributionID"],"SourceArchive"->archive["SourceArchive"],"ArchiveMember"->entry["Name"]|>;
    extracted=Join[source,<|"ArchiveSHA256"->archive["SHA256"],"MemberSHA256"->extracted["SHA256"],
      "WebsiteSource"->archive["WebsiteSource"],"IdentityStatus"->"VerifiedAgainstPDSWebsite"|>];
    result=uploadSAS[bytes,source,120];bytes=None;
    If[FailureQ[result],Throw[result,stop]];
    AssociateTo[$tables,result["Token"]->Join[$tables[result["Token"]],extracted]];
    Join[result,extracted],stop],
    bytes=None;deleted=If[StringQ[directory],zipDeleteTracked[directory],True]
  ];
  If[!deleted,
    Return[failure["ExtractionCleanupFailed","The temporary extraction directory could not be removed. Disconnect before continuing.",
      <|"TemporaryDirectory"->directory,"TableToken"->If[AssociationQ[result],Lookup[result,"Token",None],None]|>]]];
  If[AssociationQ[result],Append[result,"ExtractedDirectoryDeleted"->True],result]
];
PDSNativeLoadZIPMember[token_String,index_Integer] := run[zipLoadMember[token,index]];
zipClose[token_String] := Module[{record},
  record=Lookup[$zipArchives,token,None];
  If[!AssociationQ[record],Return[failure["UnknownArchive","The ZIP is not open in this session."]]];
  If[!zipDeleteTracked[record["Directory"]],Return[failure["ZIPCleanupFailed","The temporary ZIP directory could not be removed.",<|"TemporaryDirectory"->record["Directory"]|>]]];
  KeyDropFrom[$zipArchives,token];<|"Status"->"Closed","TemporaryZIPDeleted"->True|>
];
PDSNativeCloseZIP[token_String] := run[zipClose[token]];
zipCleanupResources[] := Module[{results,answer,website="AlreadyClosed",problems,pending},
  results=zipClose /@ Keys[$zipArchives];
  pending=Select[Keys[$zipPendingDirectories],!zipDeleteTracked[#] &];
  If[AssociationQ[$zipWebsite],
    answer=zipRequest["POST","/api/logoff"];
    If[FailureQ[answer],website=answer,website="Confirmed";$zipWebsite=None]
  ];
  problems=Select[results,FailureQ];
  If[problems=!={} || pending=!={} || FailureQ[website],$zipCleanupBlocked=True;Return[failure["ZIPCleanupIncomplete","SAS is closed, but ZIP or website cleanup needs another attempt. Click Disconnect again.",
    <|"ArchiveCleanupFailures"->problems,"TemporaryDirectories"->pending,"WebsiteLogout"->If[FailureQ[website],"Unconfirmed",website]|>]]];
  $zipCleanupBlocked=False;
  <|"TemporaryZIPsDeleted"->Length[results],"WebsiteLogout"->website|>
];
End[];
EndPackage[];
