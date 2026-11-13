Get[FileNameJoin[{DirectoryName[$InputFileName], "SEPIAModel.wl"}]];

config = <|"ExposureDays" -> "TRTDURD", "SequenceFeatures" -> {"ALT", "CREAT"}, "Epochs" -> 60|>;
data = SEPIASyntheticData[1500, 7];
profiles = SEPIABuildProfiles[data["ADSL"], data["ADAE"], data["SEQ"], config];
split = SEPIASplitSubjects[profiles, {0.7, 0.15, 0.15}, 1234];
ensemble = SEPIAFitEnsemble[profiles, split, config];

Print["Weights: ", ensemble["Weights"]];
Print["Validation log loss: ", ensemble["ValidationLogLoss"]];
Print["Poisson ridge: ", ensemble["Poisson"]["Ridge"]];

evaluation = SEPIAEvaluate[ensemble, SEPIASubset[profiles, split["Test"]]];
Scan[Print[#, ": ", evaluation[#]] &, Keys[evaluation]];

patient = <|"AGE" -> 62, "SEX" -> "M", "RACE" -> "WHITE", "Exposure" -> 90, "Sequence" -> {{30., 0.9}, {31., 0.9}}|>;
Scan[Print, SEPIARankSideEffects[ensemble, patient, "DrugA", 5]];

directory = FileNameJoin[{DirectoryName[$InputFileName], "sepia_ensemble"}];
SEPIASaveEnsemble[ensemble, directory];
Print["Saved to ", directory];
