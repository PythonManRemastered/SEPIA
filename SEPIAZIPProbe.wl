BeginPackage["SEPIAZIPProbe`"];
PDSZIPCheck;
Begin["`Private`"];
$host = "https://data.projectdatasphere.org";
$studyUUID = "6d8d63af-fc7a-40d4-a6ef-f0887f4f7a5d";
$studyUID = "Breast_EliLill_2008_168";
$archiveName = "adam20161103.zip";
$expectedBytes = 15882606;
fail[tag_, text_] := Failure[tag, <|"MessageTemplate" -> text|>];
json[bytes_ByteArray] := Quiet[Check[ImportByteArray[bytes, "RawJSON"], $Failed]];
json[_] := $Failed;
cookieForPDS[c_] := AssociationQ[c] && StringQ[Lookup[c, "Name", None]] &&
  StringQ[Lookup[c, "Content", None]] && MemberQ[
    {"data.projectdatasphere.org", ".data.projectdatasphere.org", "projectdatasphere.org", ".projectdatasphere.org"},
    ToLowerCase[Lookup[c, "Domain", ""]]];
performHTTP[req_, capture_] := Quiet[Check[URLRead[req, {"StatusCode", "BodyByteArray", "Cookies"},
  TimeConstraint -> 120, FollowRedirects -> False, VerifySecurityCertificates -> True,
  Interactive -> False, CookieFunction -> capture], $Failed]];
transport[req_] := Module[{received = {}, response, capture},
  capture = Function[cookie, If[cookieForPDS[cookie], AppendTo[received, cookie]]; Null];
  response = performHTTP[req, capture];
  If[!AssociationQ[response], Return[response]];
  Append[response, "Cookies" -> DeleteDuplicatesBy[Join[received,
      Select[Replace[Lookup[response, "Cookies", {}], Except[_List] -> {}], cookieForPDS]],
    {Lookup[#, "Name", ""], Lookup[#, "Domain", ""], Lookup[#, "Path", ""]} &]]
];
request[method_, route_, body_, cookies_] := Module[{r, status},
  r = transport[HTTPRequest[$host <> route, <|"Method" -> method,
    "Headers" -> <|"Accept" -> "application/json, application/zip, application/octet-stream",
      "Content-Type" -> "application/json"|>, "Cookies" -> cookies, "Body" -> body|>]];
  If[!AssociationQ[r], Return[fail["TransportFailure", "No complete PDS website response was received. No retry was made."]]];
  status = Lookup[r, "StatusCode", None];
  If[!IntegerQ[status], Return[fail["InvalidResponse", "PDS returned no HTTP status."]]];
  If[status == 401 || status == 403, Return[fail["WebsiteAccessDenied",
    "PDS rejected website authentication or file access. Use the PDS website email and password; SAS credentials are separate."]]];
  If[300 <= status < 400, Return[fail["WebsiteRedirect", "PDS returned a redirect. No credentials were forwarded and no retry was made."]]];
  If[status < 200 || status >= 300, Return[fail["WebsiteHTTPFailure", "PDS returned an unsuccessful HTTP status. No response body is displayed."]]];
  r
];
mergeCookies[old_List, response_Association] := Module[{new},
  new = Lookup[response, "Cookies", {}];
  If[!ListQ[new], Return[old]];
  new = Select[new, AssociationQ[#] && StringQ[Lookup[#, "Name", None]] &&
    StringQ[Lookup[#, "Content", None]] &];
  DeleteDuplicatesBy[Join[new, old], {Lookup[#, "Name", ""], Lookup[#, "Domain", ""], Lookup[#, "Path", ""]} &]
];
credentials[] := If[$FrontEnd === Null, fail["FrontEndRequired", "Run this ZIP check in a desktop Wolfram notebook."],
  DialogInput[DynamicModule[{email = "", password = ""}, Column[{
    Style["Connect to the PDS website", Bold],
    "Use your PDS website account, rather than your SAS username.",
    Grid[{{"PDS Email:", InputField[Dynamic[email], String]},
      {"PDS Password:", InputField[Dynamic[password], String, FieldMasked -> True]}}, Alignment -> Left],
    Row[{Button["Connect", With[{v = {StringTrim[email], password}},
      email = ""; password = ""; DialogReturn[v]]], Spacer[15],
      Button["Cancel", email = ""; password = ""; DialogReturn[$Canceled]]}]
  }, Spacings -> 1]], WindowTitle -> "SEPIA PDS Website Login"]];
archiveEntry[data_] := Module[{groups, files, matches, f},
  If[!AssociationQ[data] || Lookup[data, "uniqueDatasetId", ""] =!= $studyUID ||
    Lookup[data, "uuid", ""] =!= $studyUUID || Lookup[data, "donationId", None] =!= 168,
    Return[fail["StudyMismatch", "PDS returned a different contribution than the ZIP check requested."]]];
  groups = Lookup[data, "fileGroups", None];
  If[!ListQ[groups] || !AllTrue[groups, AssociationQ], Return[fail["InvalidFileListing", "PDS returned no recognized file groups."]]];
  files = Flatten[Lookup[#, "files", {}] & /@ groups];
  matches = Select[files, AssociationQ[#] && Lookup[#, "filename", ""] === $archiveName &];
  If[Length[matches] =!= 1, Return[fail["ArchiveNotUnique", "The expected ZIP was not uniquely listed by PDS."]]];
  f = First[matches];
  If[!TrueQ[Lookup[f, "downloadable", False]], Return[fail["DownloadNotAuthorized",
    "PDS does not mark this ZIP as downloadable for this website session. Check account access and any required attestation on the PDS website."]]];
  If[Lookup[f, "id", None] =!= 1003 || Lookup[f, "donationId", None] =!= 168 ||
    Lookup[f, "fileSize", None] =!= $expectedBytes,
    Return[fail["ArchiveChanged", "The ZIP identifier or size changed. No download was attempted; review the updated source metadata first."]]];
  f
];
safeMember[s_String] := s =!= "" && !StringStartsQ[s, "/"] &&
  !StringContainsQ[s, {"\\", ":"}] &&
  AllTrue[ToCharacterCode[s], # >= 32 && # =!= 127 &] &&
  !MemberQ[StringSplit[s, "/", All], ".."];
safeMember[_] := False;
zipNames[path_] := Quiet[Check[Import[path, {"ZIP", "FileNames"}], $Failed]];
checkNames[names_] := If[!ListQ[names] || !AllTrue[names, safeMember] ||
    !DuplicateFreeQ[ToLowerCase /@ names],
  fail["UnsafeOrInvalidArchive", "The ZIP directory could not be read safely or contains ambiguous member names. Nothing was extracted."], names];
writeBytes[path_, bytes_ByteArray] := Module[{stream, result},
  stream = Quiet[Check[OpenWrite[path, BinaryFormat -> True], $Failed]];
  If[!MatchQ[stream, _OutputStream], Return[fail["TemporaryFileFailure", "Could not open a temporary ZIP file."]]];
  result = WithCleanup[Null,
    Quiet[Check[BinaryWrite[stream, Normal[bytes], "Byte"], $Failed]],
    Quiet[Close[stream]]];
  If[result === $Failed, fail["TemporaryFileFailure", "Could not write the temporary ZIP file."], path]
];
deleteTemporary[path_String] := If[FileExistsQ[path], Quiet[Check[DeleteFile[path]; !FileExistsQ[path], False]], True];
deleteTemporary[_] := True;
runCheck[input_, handler_: None] := Module[{creds = input, cookies = {}, r, data, entry, bytes, path = None,
    result, deleted = True, logout = "NotStarted", attempted = False, started = AbsoluteTime[],
    loginUUID = None, stopTag, diagnostics = <|"ProbeVersion" -> "0.2.0"|>},
  If[creds === $Canceled, Return[<|"Status" -> "Canceled", "DownloadAttempted" -> False|>]];
  If[FailureQ[creds], Return[creds]];
  If[!MatchQ[creds, {_String, _String}] || StringTrim[creds[[1]]] === "" || creds[[2]] === "",
    Return[fail["InvalidCredentials", "Enter a nonempty PDS website email and password."]]];
  result = WithCleanup[Null,
    Catch[
      r = request["GET", "/api/public/user/session", "", cookies];
      If[FailureQ[r], Throw[r, stopTag]];
      cookies = mergeCookies[cookies, r];
      diagnostics["BootstrapCookiesReceived"] = Length[Lookup[r, "Cookies", {}]];
      attempted = True;
      r = request["POST", "/api/auth/login", ExportString[<|"username" -> StringTrim[creds[[1]]],
        "password" -> creds[[2]]|>, "RawJSON"], cookies];
      creds = None;
      If[FailureQ[r], Throw[r, stopTag]];
      cookies = mergeCookies[cookies, r];
      diagnostics["LoginCookiesReceived"] = Length[Lookup[r, "Cookies", {}]];
      diagnostics["CookiesRetainedAfterLogin"] = Length[cookies];
      data = json[Lookup[r, "BodyByteArray", None]];
      If[!AssociationQ[data] || !StringQ[Lookup[data, "uuid", None]] || data["uuid"] === "",
        Throw[fail["WebsiteLoginUnconfirmed", "PDS website login was not confirmed. No download was attempted."], stopTag]];
      If[!TrueQ[Lookup[data, "attested", False]], Throw[fail["WebsiteAttestationRequired",
        "Complete any required attestation on the PDS website, then run this check again. No download was attempted."], stopTag]];
      loginUUID = data["uuid"];
      diagnostics["LoginAttested"] = TrueQ[Lookup[data, "attested", False]];
      r = request["GET", "/api/public/user/session", "", cookies];
      If[FailureQ[r], Throw[r, stopTag]];
      cookies = mergeCookies[cookies, r];
      data = json[Lookup[r, "BodyByteArray", None]];
      diagnostics["PostLoginSessionHTTPStatus"] = r["StatusCode"];
      diagnostics["SessionResponseIsJSON"] = AssociationQ[data];
      diagnostics["SessionHasAuthenticatedUser"] = AssociationQ[data] &&
        StringQ[Lookup[data, "uuid", None]] && Lookup[data, "uuid", ""] =!= "";
      diagnostics["SessionMatchesLogin"] = AssociationQ[data] && Lookup[data, "uuid", None] === loginUUID;
      diagnostics["SessionAttested"] = AssociationQ[data] && TrueQ[Lookup[data, "attested", False]];
      If[!TrueQ[diagnostics["SessionMatchesLogin"]], Throw[fail["WebsiteSessionNotRetained",
        "The login response succeeded, but the following request did not retain the same authenticated PDS session. No download was attempted."], stopTag]];
      If[!TrueQ[diagnostics["SessionAttested"]], Throw[fail["WebsiteAttestationRequired",
        "The retained website session requires attestation. Complete it on PDS; no download was attempted."], stopTag]];
      Print["PDS website session verified; checking ZIP access..."];
      r = request["GET", "/api/accessdata/results/" <> $studyUUID, "", cookies];
      If[FailureQ[r], Throw[r, stopTag]];
      cookies = mergeCookies[cookies, r];
      data = json[Lookup[r, "BodyByteArray", None]];
      diagnostics["MetadataHTTPStatus"] = r["StatusCode"];
      diagnostics["MetadataResponseIsJSON"] = AssociationQ[data];
      If[AssociationQ[data],
        entry = Cases[Lookup[data, "fileGroups", {}], a_Association /;
          Lookup[a, "filename", None] === $archiveName, Infinity];
        diagnostics["MatchingArchiveEntries"] = Length[entry];
        If[Length[entry] === 1, diagnostics["ArchiveDownloadable"] = TrueQ[Lookup[First[entry], "downloadable", False]]]
      ];
      entry = archiveEntry[data];
      If[FailureQ[entry], Throw[entry, stopTag]];
      Print["Downloading adam20161103.zip temporarily; waiting for PDS..."];
      r = request["GET", "/api/file/download/168/1003", "", cookies];
      If[FailureQ[r], Throw[r, stopTag]];
      cookies = mergeCookies[cookies, r];
      bytes = Lookup[r, "BodyByteArray", None];
      If[!MatchQ[bytes, _ByteArray] || Length[bytes] =!= $expectedBytes,
        Throw[fail["ArchiveLengthMismatch", "The ZIP download was incomplete or differed from its declared size. No archive members were opened."], stopTag]];
      If[Take[Normal[bytes], 4] =!= {80, 75, 3, 4},
        Throw[fail["NotZIP", "PDS did not return the expected ZIP file. No response body is displayed."], stopTag]];
      path = FileNameJoin[{$TemporaryDirectory, "sepia_zip_check_" <> CreateUUID[] <> ".zip"}];
      r = writeBytes[path, bytes];
      bytes = None;
      If[FailureQ[r], Throw[r, stopTag]];
      data = checkNames[zipNames[path]];
      If[FailureQ[data], Throw[data, stopTag]];
      If[handler =!= None,
        entry = handler[path, data];
        If[FailureQ[entry], Throw[entry, stopTag]];
        If[!AssociationQ[entry], Throw[fail["InvalidArchiveHandler", "The ZIP member operation returned no result."], stopTag]];
        Throw[Join[<|"ContributionID" -> $studyUID, "SourceFile" -> $archiveName,
          "DownloadedBytes" -> FileByteCount[path], "MemberCount" -> Length[data]|>, entry], stopTag]
      ];
      <|"Status" -> "ZIPListed", "ContributionID" -> $studyUID, "SourceFile" -> $archiveName,
        "DownloadedBytes" -> FileByteCount[path], "MemberCount" -> Length[data],
        "Members" -> data, "MembersExtracted" -> 0|>
    , stopTag
    ],
    creds = None;
    deleted = deleteTemporary[path];
    If[attempted && cookies =!= {},
      r = request["POST", "/api/logoff", "", cookies];
      logout = If[FailureQ[r], "Unconfirmed", "Confirmed"],
      logout = If[attempted, "Unconfirmed", "NotStarted"]];
    cookies = {};
  ];
  data = <|"Diagnostics" -> diagnostics, "TemporaryFileDeleted" -> deleted, "WebsiteLogout" -> logout,
    "ElapsedSeconds" -> Round[AbsoluteTime[] - started, .001]|>;
  If[!TrueQ[deleted], data = Append[data, "TemporaryFile" -> path]];
  If[AssociationQ[result], Join[result, data],
    If[FailureQ[result], Failure[result[[1]], Join[result[[2]], data]], result]]
];
PDSZIPCheck[] := runCheck[credentials[]];
End[];
EndPackage[];
