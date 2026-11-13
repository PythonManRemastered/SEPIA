Get[FileNameJoin[{DirectoryName[$InputFileName], "SEPIAModel.wl"}]];

config = <|"ExposureDays" -> "TRTDURD", "SequenceFeatures" -> {"ALT", "CREAT"}, "Epochs" -> 40, "Hidden" -> 24|>;
data = SEPIASyntheticData[1500, 7];
profiles = SEPIABuildProfiles[data["ADSL"], data["ADAE"], data["SEQ"], config];
split = SEPIASplitSubjects[profiles, {0.7, 0.15, 0.15}, 1234];
ensemble = SEPIAFitEnsemble[profiles, split, config];
evaluation = SEPIAEvaluate[ensemble, SEPIASubset[profiles, split["Test"]]];

directory = FileNameJoin[{$TemporaryDirectory, "sepia_ensemble_test"}];
SEPIASaveEnsemble[ensemble, directory];
reloaded = SEPIALoadEnsemble[directory];

patient = <|"AGE" -> 62, "SEX" -> "M", "RACE" -> "WHITE", "Exposure" -> 90, "Sequence" -> {{30., 0.9}, {31., 0.9}}|>;
original = SEPIARankSideEffects[ensemble, patient, "DrugA", 8];
restored = SEPIARankSideEffects[reloaded, patient, "DrugA", 8];
otherDrug = SEPIARankSideEffects[ensemble, patient, "DrugB", 8];

jsonFile = FileNameJoin[{$TemporaryDirectory, "sepia_request.json"}];
Export[jsonFile, <|"drug" -> "DrugA", "patient" -> patient|>, "RawJSON"];
jsonResult = ImportString[SEPIARankFromJSON[directory, jsonFile, 5], "RawJSON"];

poissonError = Module[{n = 5000, z, x, y, beta},
   SeedRandom[3];
   z = RandomVariate[NormalDistribution[0, 1], n];
   x = Transpose[{ConstantArray[1., n], z}];
   y = N[Map[Function[l, RandomVariate[PoissonDistribution[l]]], Exp[0.3 + 0.5 z]]];
   beta = SEPIAModel`Private`poissonFit[x, y, ConstantArray[0., n], 0.001];
   Max[Abs[beta - {0.3, 0.5}]]];

tests = {
   VerificationTest[Length[profiles], 1500, TestID -> "profile-count"],
   VerificationTest[
    {Intersection[split["Train"], split["Validation"]], Intersection[split["Train"], split["Test"]], Intersection[split["Validation"], split["Test"]]},
    {{}, {}, {}}, TestID -> "split-disjoint"],
   VerificationTest[Sort[Join[split["Train"], split["Validation"], split["Test"]]] === Sort[profiles[[All, "ID"]]], True, TestID -> "split-complete"],
   VerificationTest[poissonError < 0.1, True, TestID -> "poisson-recovers-coefficients"],
   VerificationTest[AssociationQ[ensemble], True, TestID -> "ensemble-trained"],
   VerificationTest[!MissingQ[ensemble["RNN"]], True, TestID -> "rnn-trained"],
   VerificationTest[ensemble["RNNStatus"], "Used", TestID -> "rnn-predicts"],
   VerificationTest[AllTrue[Flatten[Map[Values, Values[evaluation]]], NumberQ], True, TestID -> "all-metrics-numeric"],
   VerificationTest[AllTrue[Flatten[Map[Values, original[[All, "Components"]]]], NumberQ], True, TestID -> "ranking-components-numeric"],
   VerificationTest[Abs[Total[Values[ensemble["Weights"]]] - 1.] < 10.^-9, True, TestID -> "weights-sum-to-one"],
   VerificationTest[evaluation["Ensemble"]["LogLoss"] < evaluation["PrevalenceBaseline"]["LogLoss"], True, TestID -> "beats-baseline-logloss"],
   VerificationTest[evaluation["Ensemble"]["MacroAUC"] > 0.58, True, TestID -> "macro-auc-above-chance"],
   VerificationTest[evaluation["Ensemble"]["RecallAtK"] >= evaluation["PrevalenceBaseline"]["RecallAtK"], True, TestID -> "recall-at-k-not-worse-than-baseline"],
   VerificationTest[Length[original], Min[8, Length[ensemble["Vocabulary"]]], TestID -> "ranking-length"],
   VerificationTest[Sort[original[[All, "Probability"]], Greater] === original[[All, "Probability"]], True, TestID -> "ranking-sorted"],
   VerificationTest[original[[All, "SideEffect"]] =!= otherDrug[[All, "SideEffect"]], True, TestID -> "ranking-depends-on-drug"],
   VerificationTest[original[[All, "SideEffect"]] === restored[[All, "SideEffect"]], True, TestID -> "save-load-same-order"],
   VerificationTest[Max[Abs[original[[All, "Probability"]] - restored[[All, "Probability"]]]] < 10.^-6, True, TestID -> "save-load-same-probabilities"],
   VerificationTest[Length[jsonResult], 5, TestID -> "json-ranking-length"],
   VerificationTest[FailureQ[SEPIARankSideEffects[ensemble, patient, "NotADrug", 3]], True, TestID -> "unknown-drug-rejected"]};

report = TestReport[tests];
Print[report["TestsSucceededCount"], " passed, ", report["TestsFailedCount"], " failed"];
Scan[Print[#["TestID"], ": ", #["Outcome"]] &, Values[report["TestResults"]]];
Print[evaluation];
Print[ensemble["Weights"]];
