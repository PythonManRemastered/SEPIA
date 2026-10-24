contribution = SEPIAImport`RegisterLocalContribution[
  "Multiple_Brigham_454",
  "https://data.projectdatasphere.org/projectdatasphere/html/content/334bbb4b-728a-4b5c-a9ea-fcbfde7d2611",
  FileNameJoin[{$HomeDirectory, "Documents", "SEPIA", "AllProvidedFiles_454"}],
  <|"Title" -> "Vitamin D and Omega-3 Trial (VITAL)", "ClinicalTrialsGovID" -> "NCT01169259"|>];
lungContribution = SEPIAImport`RegisterLocalContribution[
  "LungSm_G1Thera_2017_435",
  "https://data.projectdatasphere.org/projectdatasphere/html/content/8c0ddd30-2fe6-4546-ba89-edf852cb16bf",
  FileNameJoin[{$HomeDirectory, "Documents", "SEPIA", "AllProvidedFiles_435"}],
  <|"ClinicalTrialsGovID" -> "NCT03041311", "ReportedStudyArms" -> "Comparator arm data only",
    "ReportedPatientCounts" -> <|"PatientsInDataset" -> 107, "Control" -> 53, "Experimental" -> 0|>,
    "PatientCountDiscrepancy" -> "Unresolved"|>];
breastContribution = SEPIAImport`RegisterLocalContribution[
  "Breast_EliLill_2008_168",
  "https://data.projectdatasphere.org/projectdatasphere/html/content/6d8d63af-fc7a-40d4-a6ef-f0887f4f7a5d",
  FileNameJoin[{$HomeDirectory, "Documents", "SEPIA", "AllProvidedFiles_168"}],
  <|"ClinicalTrialsGovID" -> "NCT00703326", "ReportedStudyArms" -> "Comparator arm data only",
    "ReportedPatientCounts" -> <|"PatientsInDataset" -> 1144, "Control" -> 385, "Experimental" -> 0|>,
    "PatientCountDiscrepancy" -> "Unresolved"|>];
catalog = SEPIACatalog`CreateContributionCatalog[{contribution, lungContribution, breastContribution}];
If[FailureQ[catalog], catalog, Keys[catalog["Contributions"]]]
