BeginPackage["SEPIAImport`"];
RegisterLocalContribution;
ListContributionFiles;
InspectFile;
ImportContributionFile;
Begin["`Private`"];
fail[tag_, message_] := Failure[tag, <|"MessageTemplate" -> message|>];
valid[c_] := AssociationQ[c] && Lookup[c, "RecordType", None] === "SEPIALocalContribution" &&
  StringQ[Lookup[c, "RootDirectory", None]] && StringQ[Lookup[c, "ContributionID", None]];
RegisterLocalContribution[id_String, url_String, folder_String, metadata_: <||>] := Module[{root},
  If[StringTrim[id] === "" || StringTrim[url] === "" || !AssociationQ[metadata],
    Return[fail["InvalidRegistration", "Supply a nonempty identifier, source URL and metadata association."]]];
  root = ExpandFileName[folder];
  If[!DirectoryQ[root], Return[fail["FolderNotFound", "The local contribution directory does not exist."]]];
  <|"RecordType" -> "SEPIALocalContribution", "ContributionID" -> id,
    "IdentifierStatus" -> "UserSuppliedNotVerified", "SourceURL" -> url,
    "AccessMethod" -> "ManualDownloadLocalRegistration", "RootDirectory" -> root,
    "Metadata" -> metadata, "RegisteredAt" -> DateString[]|>
];
ListContributionFiles[c_] := Module[{root, paths, relative},
  If[!valid[c], Return[fail["InvalidContribution", "Pass a successful registration record."]]];
  root = c["RootDirectory"];
  If[!DirectoryQ[root], Return[fail["FolderNotFound", "The registered directory is no longer available."]]];
  paths = Quiet@Check[Select[FileNames["*", root, Infinity], FileType[#] === File &], $Failed];
  If[paths === $Failed, Return[fail["InventoryFailed", "The local file inventory could not be read."]]];
  Map[Function[p,
    relative = FileNameJoin[Drop[FileNameSplit[p], Length[FileNameSplit[root]]]];
    <|"RelativePath" -> relative, "Name" -> FileNameTake[p],
      "Extension" -> ToLowerCase[FileExtension[p]],
      "Bytes" -> Quiet@Check[FileByteCount[p], Missing["Unavailable"]],
      "Availability" -> "Local", "SupportedImport" -> MemberQ[{"xlsx", "sas7bdat"}, ToLowerCase[FileExtension[p]]]|>
  ], Sort[paths]]
];
resolve[c_, relative_String] := Module[{inventory, matches},
  inventory = ListContributionFiles[c]; If[FailureQ[inventory], Return[inventory]];
  matches = Select[inventory, #["RelativePath"] === relative &];
  If[Length[matches] =!= 1, Return[fail["FileNotFound", "Select an exact RelativePath from ListContributionFiles."]]];
  FileNameJoin[{c["RootDirectory"], relative}]
];

SetAttributes[bounded, HoldFirst];
bounded[expr_, seconds_, bytes_] := TimeConstrained[
  MemoryConstrained[Quiet@Check[expr, fail["ReadFailed", "Wolfram emitted a message while reading this file; no import was accepted."]],
    bytes, fail["MemoryLimit", "The operation exceeded its memory allowance."]],
  seconds, fail["TimeLimit", "The operation exceeded its time allowance."]];
InspectFile[c_, relative_String] := Module[{path, ext, sheets, metadata},
  path = resolve[c, relative]; If[FailureQ[path], Return[path]];
  ext = ToLowerCase[FileExtension[path]];
  If[ext === "sas7bdat",
    metadata = sasMetadata[path, 60, 512*1024^2];
    If[FailureQ[metadata], Return[metadata]];
    Return[<|"RelativePath" -> relative, "Extension" -> ext, "SupportedImport" -> True,
      "Sheets" -> Missing["NotApplicable"], "FileMetadata" -> metadata|>]];
  If[ext =!= "xlsx", Return[<|"RelativePath" -> relative, "Extension" -> ext,
    "SupportedImport" -> False, "Sheets" -> Missing["NotApplicable"]|>]];
  sheets = bounded[Import[path, {"XLSX", "Sheets"}], 60, 512*1024^2];
  If[FailureQ[sheets], Return[sheets]];
  If[!MatchQ[sheets, {__String}], Return[fail["InvalidWorkbook", "No usable sheet list was returned."]]];
  <|"RelativePath" -> relative, "Extension" -> ext, "SupportedImport" -> True, "Sheets" -> sheets|>
];
Options[ImportContributionFile] = {"FirstRowIsHeader" -> False, "PreviewRows" -> 0,
  "PreviewColumns" -> 10, "MaxFileBytes" -> 256*1024^2, "TimeLimit" -> 120, "MemoryLimit" -> 1024^3};
ImportContributionFile[c_, relative_String, sheet_String, OptionsPattern[]] := Module[
  {path, info, size, before, after, raw, widths, header, rows, warnings, preview,
   h = OptionValue["FirstRowIsHeader"], n = OptionValue["PreviewRows"],
   cols = OptionValue["PreviewColumns"], max = OptionValue["MaxFileBytes"],
   seconds = OptionValue["TimeLimit"], memory = OptionValue["MemoryLimit"]},
  If[!MemberQ[{True, False}, h] || !IntegerQ[n] || !Between[n, {0, 20}] ||
    !IntegerQ[cols] || !Between[cols, {1, 50}] || !IntegerQ[max] || max <= 0 ||
    !NumericQ[seconds] || !TrueQ[seconds > 0] || !IntegerQ[memory] || memory <= 0,
    Return[fail["InvalidOptions", "Use Boolean header selection, 0-20 preview rows, 1-50 preview columns, and positive resource limits."]]];
  path = resolve[c, relative]; If[FailureQ[path], Return[path]];
  If[ToLowerCase[FileExtension[path]] === "sas7bdat",
    Return[fail["InvalidSelection", "SAS has no worksheet selection; pass None as the third argument."]]];
  If[ToLowerCase[FileExtension[path]] =!= "xlsx", Return[fail["UnsupportedFormat", "Version 0.2 imports XLSX and SAS7BDAT only."]]];
  size = Quiet@Check[FileByteCount[path], $Failed];
  If[!IntegerQ[size], Return[fail["ReadFailed", "File size could not be read."]]];
  If[size > max, Return[fail["FileSizeLimit", "The selected file exceeds MaxFileBytes."]]];
  before = bounded[FileHash[path, "SHA256"], seconds, memory];
  If[FailureQ[before], Return[before]];
  info = InspectFile[c, relative]; If[FailureQ[info], Return[info]];
  If[!MemberQ[info["Sheets"], sheet], Return[fail["SheetNotFound", "Select an exact sheet name returned by InspectFile."]]];
  raw = bounded[Import[path, {"XLSX", "Data", sheet}], seconds, memory];
  If[FailureQ[raw], Return[raw]];
  If[!ListQ[raw] || raw === {} || !AllTrue[raw, ListQ] || Total[Length /@ raw] === 0,
    Return[fail["EmptyOrInvalidTable", "The selected sheet did not produce a nonempty row table."]]];
  widths = Union[Length /@ raw];
  If[Length[widths] =!= 1, Return[fail["RaggedTable", "The imported rows have inconsistent widths."]]];
  after = bounded[FileHash[path, "SHA256"], seconds, memory];
  If[FailureQ[after], Return[after]];
  If[before =!= after, Return[fail["SourceChanged", "The source file changed during inspection/import; retry on a stable copy."]]];
  header = If[h, First[raw], Missing["NotDeclared"]]; rows = If[h, Rest[raw], raw];
  warnings = {"Data contains the full imported sheet, including any header row. No clinical recoding was performed.",
    "XLSX Data import returns interpreted cell values, not a lossless copy of formatting, formulas or all workbook metadata. Source fidelity still requires verification.",
    "PDS identifier and metadata are user supplied; remote file completeness and clinical suitability are unverified."};
  If[h && !DuplicateFreeQ[header], AppendTo[warnings, "Duplicate header values retained; no row-to-association conversion performed."]];
  preview = If[n == 0, Missing["Disabled"], Take[#, UpTo[cols]] & /@ Take[rows, UpTo[n]]];
  <|"Data" -> raw,
    "Provenance" -> <|"ContributionID" -> c["ContributionID"], "IdentifierStatus" -> c["IdentifierStatus"],
      "SourceURL" -> c["SourceURL"], "RelativePath" -> relative, "Sheet" -> sheet,
      "SHA256" -> IntegerString[before, 16, 64], "FileBytes" -> size,
      "ImportedAt" -> DateString[], "TimeZone" -> $TimeZone, "WolframVersion" -> $Version,
      "PackageVersion" -> "0.2.0", "ImportElements" -> {"XLSX", "Data", sheet},
      "ImportOptions" -> {}, "FirstRowIsHeader" -> h|>,
    "SourceMetadata" -> c["Metadata"],
    "Diagnostics" -> <|"ImportedRowCount" -> Length[raw], "DataRowCount" -> Length[rows],
      "ColumnCount" -> First[widths], "Header" -> header,
      "DuplicateHeaderValues" -> If[h, Keys[Select[Counts[header], # > 1 &]], Missing["NotDeclared"]]|>,
    "Preview" -> preview, "Warnings" -> warnings|>
];

sasMetadata[path_, seconds_, memory_] := Module[{elements, keys, values},
  elements = bounded[Import[path, {"SAS7BDAT", "Elements"}], seconds, memory];
  If[FailureQ[elements], Return[elements]];
  keys = {"TableName", "Dimensions", "ColumnDescriptions", "ColumnLabels", "ColumnTypes",
    "CharacterEncoding", "Compression", "MissingValues"};
  If[!ListQ[elements] || !AllTrue[Join[keys, {"Data", "MissingPositions"}], MemberQ[elements, #] &],
    Return[fail["MissingSASElements", "Required SAS data or metadata elements are unavailable."]]];
  values = AssociationMap[bounded[Import[path, {"SAS7BDAT", #}], seconds, memory] &, keys];
  If[AnyTrue[Values[values], FailureQ], Return[First[Select[Values[values], FailureQ]]]];
  values
];
sasAudit[data_, names_, reported_] := Module[{checks, counts},
  If[!AssociationQ[reported] || !AllTrue[Keys[reported], MemberQ[names, #] &],
    Return[fail["InvalidMissingMetadata", "Missing-position metadata contains unknown columns or is not an association."]]];
  checks = MapIndexed[Function[{name, index}, Module[{column, positions, rules, supplied},
    column = data[[All, First[index]]];
    positions = Flatten[Position[column, _Missing, {1}]];
    rules = (# -> column[[#]]) & /@ positions;
    supplied = Lookup[reported, name, {}];
    If[!MatchQ[supplied, {Rule[_Integer, _Missing] ...}], Return[False]];
    SortBy[supplied, First] === SortBy[rules, First]
  ]], names];
  If[!AllTrue[checks, TrueQ], Return[fail["MissingAuditFailed", "Imported missing positions or markers disagree with metadata, or its representation is unsupported."]]];
  counts = AssociationThread[names, Count[#, _Missing] & /@ Transpose[data]];
  <|"ColumnsChecked" -> Length[names], "AllPositionsAndMarkersAgree" -> True,
    "MissingCounts" -> Select[counts, # > 0 &],
    "MissingForms" -> DeleteDuplicates[Cases[data, _Missing, {2}]]|>
];
ImportContributionFile[c_, relative_String, None, OptionsPattern[]] := Module[
  {path, size, before, after, metadata, names, labels, raw, positions, audit, preview,
   h = OptionValue["FirstRowIsHeader"], n = OptionValue["PreviewRows"],
   cols = OptionValue["PreviewColumns"], max = OptionValue["MaxFileBytes"],
   seconds = OptionValue["TimeLimit"], memory = OptionValue["MemoryLimit"]},
  If[h =!= False, Return[fail["InvalidHeaderOption", "SAS column names are metadata; FirstRowIsHeader must be False."]]];
  If[!IntegerQ[n] || !Between[n, {0, 20}] || !IntegerQ[cols] || !Between[cols, {1, 50}] ||
    !IntegerQ[max] || max <= 0 || !NumericQ[seconds] || !TrueQ[seconds > 0] ||
    !IntegerQ[memory] || memory <= 0,
    Return[fail["InvalidOptions", "Use 0-20 preview rows, 1-50 preview columns and positive resource limits."]]];
  path = resolve[c, relative]; If[FailureQ[path], Return[path]];
  If[ToLowerCase[FileExtension[path]] =!= "sas7bdat",
    Return[fail["InvalidSelection", "None selects the single SAS table; XLSX requires an explicit sheet name."]]];
  size = bounded[FileByteCount[path], seconds, memory]; If[FailureQ[size], Return[size]];
  If[!IntegerQ[size], Return[fail["ReadFailed", "File size could not be read."]]];
  If[size > max, Return[fail["FileSizeLimit", "The selected file exceeds MaxFileBytes."]]];
  before = bounded[FileHash[path, "SHA256"], seconds, memory]; If[FailureQ[before], Return[before]];
  metadata = sasMetadata[path, seconds, memory]; If[FailureQ[metadata], Return[metadata]];
  If[!MatchQ[metadata["Dimensions"], {_Integer?Positive, _Integer?Positive}] ||
    !ListQ[metadata["ColumnDescriptions"]] || !AllTrue[metadata["ColumnDescriptions"], AssociationQ],
    Return[fail["InvalidSASMetadata", "Expected positive table dimensions and column description associations."]]];
  names = Lookup[metadata["ColumnDescriptions"], "Name", Missing["NoName"]];
  labels = metadata["ColumnLabels"];
  If[!VectorQ[names, StringQ] || !DuplicateFreeQ[names] ||
    Length[names] =!= Last[metadata["Dimensions"]] || names =!= labels,
    Return[fail["InvalidSASColumns", "Column names must be unique strings and agree with dimensions and ColumnLabels order."]]];
  raw = bounded[Import[path, {"SAS7BDAT", "Data"}], seconds, memory];
  If[FailureQ[raw], Return[raw]];
  If[!ListQ[raw] || !AllTrue[raw, ListQ] || Dimensions[raw] =!= metadata["Dimensions"],
    Return[fail["StructureMismatch", "Imported SAS dimensions disagree with metadata."]]];
  positions = bounded[Import[path, {"SAS7BDAT", "MissingPositions"}], seconds, memory];
  If[FailureQ[positions], Return[positions]];
  audit = bounded[sasAudit[raw, names, positions], seconds, memory];
  If[FailureQ[audit], Return[audit]];
  after = bounded[FileHash[path, "SHA256"], seconds, memory]; If[FailureQ[after], Return[after]];
  If[before =!= after, Return[fail["SourceChanged", "The source changed during import; retry on a stable copy."]]];
  preview = If[n == 0, Missing["Disabled"], Take[#, UpTo[cols]] & /@ Take[raw, UpTo[n]]];
  <|"Data" -> raw, "ColumnNames" -> names, "FileMetadata" -> metadata,
    "SourceMetadata" -> c["Metadata"],
    "Provenance" -> <|"ContributionID" -> c["ContributionID"], "IdentifierStatus" -> c["IdentifierStatus"],
      "SourceURL" -> c["SourceURL"], "RelativePath" -> relative, "Sheet" -> None,
      "TableName" -> metadata["TableName"], "SHA256" -> IntegerString[before, 16, 64],
      "FileBytes" -> size, "ImportedAt" -> DateString[], "TimeZone" -> $TimeZone,
      "WolframVersion" -> $Version, "PackageVersion" -> "0.2.0",
      "ImportElements" -> {"SAS7BDAT", "Data"}, "ImportOptions" -> {}, "FirstRowIsHeader" -> False|>,
    "Diagnostics" -> <|"ImportedRowCount" -> Length[raw], "DataRowCount" -> Length[raw],
      "ColumnCount" -> Length[names], "DuplicateHeaderValues" -> {}, "MissingValueAudit" -> audit|>,
    "Preview" -> preview,
    "Warnings" -> {"SAS Data contains records only; column names and descriptions are stored separately. No clinical recoding was performed.",
      "Missing audit checks consistency between outputs of the same Wolfram importer, not independent validation against another SAS reader.",
      "Clinical missingness meanings and custom code conventions require source documentation. This representation is not a lossless SAS-file replacement.",
      "PDS identifier and metadata are user supplied; remote completeness and clinical suitability are unverified."}|>
];

RegisterLocalContribution[___] := fail["InvalidArguments", "Expected identifier, source URL, local folder and optional metadata association."];
ListContributionFiles[___] := fail["InvalidArguments", "Expected one contribution record."];
InspectFile[___] := fail["InvalidArguments", "Expected a contribution record and relative path string."];
ImportContributionFile[___] := fail["InvalidArguments", "Expected a contribution record, relative path and selection (XLSX sheet name or None for SAS)."];
End[];
EndPackage[];
