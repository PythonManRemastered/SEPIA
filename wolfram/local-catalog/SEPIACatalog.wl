BeginPackage["SEPIACatalog`"];
CreateContributionCatalog;
LookupContribution;
Begin["`Private`"];
fail[tag_, text_] := Failure[tag, <|"MessageTemplate" -> text|>];
recordQ[r_] := AssociationQ[r] &&
  Lookup[r, "RecordType", None] === "SEPIALocalContribution" &&
  AllTrue[{"ContributionID", "SourceURL", "RootDirectory", "IdentifierStatus"},
    StringQ[Lookup[r, #, None]] && StringTrim[Lookup[r, #, ""]] =!= "" &] &&
  AssociationQ[Lookup[r, "Metadata", None]];
CreateContributionCatalog[records_List] := Module[{ids},
  If[!AllTrue[records, recordQ], Return[fail["InvalidRegistrationRecord",
    "Supply successful RegisterLocalContribution records; resolve any registration failures first."]]];
  ids = Lookup[records, "ContributionID", {}];
  If[!DuplicateFreeQ[ids], Return[fail["DuplicateContributionID",
    "Duplicate identifiers would overwrite records. Resolve them explicitly before building the catalog."]]];
  <|"CatalogType" -> "SEPIALocalCatalog", "CatalogVersion" -> "0.1.0",
    "ResolutionMethod" -> "ExplicitLocalRegistration",
    "Contributions" -> AssociationThread[ids, records]|>
];
LookupContribution[c_, id_String] := Module[{entries, record},
  If[!AssociationQ[c] || Lookup[c, "CatalogType", None] =!= "SEPIALocalCatalog" ||
    !AssociationQ[Lookup[c, "Contributions", None]],
    Return[fail["InvalidCatalog", "Supply a catalog returned by CreateContributionCatalog."]]];
  entries = c["Contributions"];
  If[!KeyExistsQ[entries, id], Return[fail["UnregisteredContribution",
    "This exact identifier is not registered locally. This does not establish whether it exists on PDS."]]];
  record = entries[id];
  If[!recordQ[record] || record["ContributionID"] =!= id,
    Return[fail["InvalidRegistrationRecord", "The selected catalog record is malformed or has an inconsistent identifier."]]];
  record
];
CreateContributionCatalog[___] := fail["InvalidArguments", "Supply a list of registration records."];
LookupContribution[___] := fail["InvalidArguments", "Supply a catalog and an exact identifier string."];
End[];
EndPackage[];
