BeginPackage["SEPIAModel`"];

SEPIADefaultConfig;
SEPIAFromImported;
SEPIABuildProfiles;
SEPIASplitSubjects;
SEPIASubset;
SEPIAFitEnsemble;
SEPIAEvaluate;
SEPIARankSideEffects;
SEPIASaveEnsemble;
SEPIALoadEnsemble;
SEPIARankFromJSON;
SEPIASyntheticData;

Begin["`Private`"];

SEPIADefaultConfig = <|
   "SubjectID" -> "USUBJID",
   "Drug" -> "TRT01A",
   "NumericFeatures" -> {"AGE"},
   "CategoricalFeatures" -> {"SEX", "RACE"},
   "ExposureDays" -> None,
   "AETerm" -> "AEDECOD",
   "SequenceTime" -> "ADY",
   "SequenceFeatures" -> {},
   "LandmarkDay" -> 0,
   "MaxSequenceLength" -> 20,
   "TopTerms" -> 15,
   "MinSubjectsPerTerm" -> 10,
   "RidgeGrid" -> {0.1, 1., 10., 100.},
   "Hidden" -> 32,
   "Epochs" -> 80,
   "Patience" -> 8,
   "Seed" -> 1234,
   "RecallK" -> 3|>;

failure[code_, message_] := Failure[code, <|"MessageTemplate" -> message|>];

cleanValue[v_] := Which[
   MissingQ[v], Missing[],
   AssociationQ[v], Lookup[v, "ISO8601", Missing[]],
   v === Null, Missing[],
   StringQ[v], If[StringTrim[v] === "", Missing[], StringTrim[v]],
   True, v];

cleanRow[row_Association] := cleanValue /@ row;

toNumber[v_] := Which[
   NumberQ[v], N[v],
   StringQ[v] && StringMatchQ[v, NumberString], N[ToExpression[v]],
   True, Missing[]];

cleanNumber[v_] := toNumber[cleanValue[v]];

toLabel[v_] := If[MissingQ[v], "Unknown", ToString[v]];

normTerm[v_] := If[MissingQ[v], Missing[], ToUpperCase[StringTrim[ToString[v]]]];

SEPIAFromImported[imported_Association] :=
  Map[Function[row, AssociationThread[imported["ColumnNames"] -> row]], imported["Data"]];

SEPIABuildProfiles[adsl_List, adae_List, seq_List, userConfig_Association : <||>] := Module[
   {c, idKey, termKey, seqKey, seqFeat, landmark, maxLen, subjects, exposures, valid, med, aeKeys, aeCounts, bySubject, seqBySubject},
   c = Join[SEPIADefaultConfig, userConfig];
   idKey = c["SubjectID"];
   termKey = c["AETerm"];
   seqKey = c["SequenceTime"];
   seqFeat = c["SequenceFeatures"];
   landmark = c["LandmarkDay"];
   maxLen = c["MaxSequenceLength"];
   subjects = DeleteDuplicatesBy[Select[cleanRow /@ adsl, !MissingQ[#[idKey]] &], ToString[#[idKey]] &];
   exposures = If[c["ExposureDays"] === None,
     ConstantArray[1., Length[subjects]],
     Map[Function[s, cleanNumber[s[c["ExposureDays"]]]], subjects]];
   valid = Select[exposures, NumberQ[#] && # > 0 &];
   med = If[valid === {}, 1., Median[valid]];
   exposures = Map[Function[e, If[NumberQ[e] && e > 0, e, med]], exposures];
   aeKeys = Select[
     Map[Function[r, {r[idKey], normTerm[r[termKey]]}], cleanRow /@ adae],
     !MissingQ[#[[1]]] && !MissingQ[#[[2]]] &];
   aeCounts = Counts[Map[Function[k, {ToString[k[[1]]], k[[2]]}], aeKeys]];
   bySubject = GroupBy[Normal[aeCounts], (#[[1, 1]] &) -> (Function[r, r[[1, 2]] -> r[[2]]]), Association];
   seqBySubject = If[seqFeat === {} || seq === {},
     <||>,
     GroupBy[
      Select[
       Map[
        Function[r,
         With[{rr = cleanRow[r]},
          <|"ID" -> If[MissingQ[rr[idKey]], Missing[], ToString[rr[idKey]]],
           "Time" -> cleanNumber[rr[seqKey]],
           "Values" -> Map[Function[f, cleanNumber[rr[f]]], seqFeat]|>]],
        seq],
       !MissingQ[#["ID"]] && NumberQ[#["Time"]] && #["Time"] <= landmark && AnyTrue[#["Values"], NumberQ] &],
      (#["ID"] &) -> (Function[r, {r["Time"], r["Values"]}]),
      Function[g, Map[Last, Take[SortBy[g, First], -Min[Length[g], maxLen]]]]]];
   If[subjects === {},
    failure["NoSubjects", "No ADSL rows with a subject identifier were found."],
    MapThread[
     Function[{s, e},
      With[{id = ToString[s[idKey]]},
       <|"ID" -> id,
        "Drug" -> toLabel[Lookup[s, c["Drug"], Missing[]]],
        "Numeric" -> Map[Function[f, cleanNumber[Lookup[s, f, Missing[]]]], c["NumericFeatures"]],
        "Categorical" -> Map[Function[f, toLabel[Lookup[s, f, Missing[]]]], c["CategoricalFeatures"]],
        "Exposure" -> e,
        "Counts" -> Lookup[bySubject, id, <||>],
        "Sequence" -> Lookup[seqBySubject, id, {}]|>]],
     {subjects, exposures}]]];

SEPIASplitSubjects[profiles_List, fractions_List : {0.7, 0.15, 0.15}, seed_Integer : 1234] := Module[
   {groups, parts, splitIds},
   SeedRandom[seed];
   groups = GroupBy[profiles, (#["Drug"] &), Function[g, g[[All, "ID"]]]];
   splitIds[ids_] := Module[{s = RandomSample[ids], n = Length[ids], a, b},
     a = If[n < 3, n, Max[1, Min[n - 2, Round[fractions[[1]] n]]]];
     b = If[n < 3, 0, Max[1, Min[n - a - 1, Round[fractions[[2]] n]]]];
     {s[[1 ;; a]], s[[a + 1 ;; a + b]], s[[a + b + 1 ;; n]]}];
   parts = splitIds /@ Values[groups];
   <|"Train" -> Join @@ parts[[All, 1]], "Validation" -> Join @@ parts[[All, 2]], "Test" -> Join @@ parts[[All, 3]]|>];

SEPIASubset[profiles_List, ids_List] := Lookup[AssociationThread[profiles[[All, "ID"]] -> profiles], ids];

vocabulary[train_, c_] := Module[{counts, ranked},
   counts = Counts[Flatten[Map[Function[p, Keys[p["Counts"]]], train]]];
   ranked = Select[ReverseSort[counts], # >= c["MinSubjectsPerTerm"] &];
   Take[Keys[ranked], UpTo[c["TopTerms"]]]];

countMatrix[profiles_, vocab_] := Table[N[Lookup[p["Counts"], t, 0]], {p, profiles}, {t, vocab}];

binaryMatrix[profiles_, vocab_] := N[Map[Boole[# > 0.] &, countMatrix[profiles, vocab], {2}]];

meanSD[col_List] := With[{v = Select[col, NumberQ]},
   {If[v === {}, 0., Mean[v]],
    If[Length[v] < 2, 1., With[{s = StandardDeviation[v]}, If[s > 0., s, 1.]]]}];

hasMissing[col_List] := AnyTrue[col, !NumberQ[#] &];

fitEncoder[train_, c_] := Module[{nums, stats, seqSteps, seqCols, seqStats, levels, drugLevels},
   nums = If[Length[c["NumericFeatures"]] == 0, {}, Transpose[train[[All, "Numeric"]]]];
   stats = meanSD /@ nums;
   seqSteps = Join @@ train[[All, "Sequence"]];
   seqCols = If[Length[c["SequenceFeatures"]] == 0 || seqSteps === {},
     ConstantArray[{}, Length[c["SequenceFeatures"]]],
     Transpose[seqSteps]];
   seqStats = meanSD /@ seqCols;
   levels = Table[Sort[DeleteDuplicates[train[[All, "Categorical", k]]]], {k, Length[c["CategoricalFeatures"]]}];
   drugLevels = Sort[DeleteDuplicates[train[[All, "Drug"]]]];
   <|"NumericMeans" -> (First /@ stats),
    "NumericSDs" -> (Last /@ stats),
    "NumericHasMissing" -> (hasMissing /@ nums),
    "CategoricalLevels" -> levels,
    "DrugLevels" -> drugLevels,
    "SeqMeans" -> (First /@ seqStats),
    "SeqSDs" -> (Last /@ seqStats),
    "ExposureRef" -> Median[train[[All, "Exposure"]]]|>];

oneHot[v_, levels_List] := Map[Function[l, Boole[l === v]], levels];

encodeRow[enc_, p_, drug_] := N[Join[
    MapThread[
     Function[{v, m, s}, If[NumberQ[v], (v - m)/s, 0.]],
     {p["Numeric"], enc["NumericMeans"], enc["NumericSDs"]}],
    Map[Function[v, Boole[!NumberQ[v]]], Pick[p["Numeric"], enc["NumericHasMissing"]]],
    Flatten[MapThread[oneHot, {p["Categorical"], enc["CategoricalLevels"]}]],
    oneHot[drug, enc["DrugLevels"]]]];

seqStep[enc_, values_] := N[Join[
    MapThread[
     Function[{v, m, s}, If[NumberQ[v], (v - m)/s, 0.]],
     {values, enc["SeqMeans"], enc["SeqSDs"]}],
    Map[Function[v, Boole[!NumberQ[v]]], values]]];

encodeSeq[enc_, seq_List] := If[seq === {},
   {ConstantArray[0., 2 Length[enc["SeqMeans"]]]},
   Map[Function[v, seqStep[enc, v]], seq]];

designMatrix[enc_, profiles_, drug_ : Automatic] :=
  N[Map[Function[p, encodeRow[enc, p, If[drug === Automatic, p["Drug"], drug]]], profiles]];

offsets[enc_, profiles_] := Map[Function[p, Log[p["Exposure"]/enc["ExposureRef"]]], profiles];

addIntercept[x_] := Map[Function[row, Prepend[row, 1.]], x];

poissonFit[x_, y_, off_, lambda_] := Module[
   {p, pen, beta, eta, mu, grad, hess, step, current, k, nll},
   p = Dimensions[x][[2]];
   pen = lambda DiagonalMatrix[Prepend[ConstantArray[1., p - 1], 0.]];
   nll[b_] := With[{e = Clip[x . b + off, {-30., 30.}]}, Total[Exp[e] - y e] + 0.5 b . pen . b];
   beta = ConstantArray[0., p];
   beta[[1]] = Log[Max[Mean[y], 10.^-6]/Mean[Exp[off]]];
   Do[
    eta = Clip[x . beta + off, {-30., 30.}];
    mu = Exp[eta];
    grad = Transpose[x] . (y - mu) - pen . beta;
    hess = Transpose[x] . (mu x) + pen + 10.^-6 IdentityMatrix[p];
    step = LinearSolve[hess, grad];
    current = nll[beta];
    k = 0;
    While[k < 25 && nll[beta + step] > current, step = step/2; k++];
    beta = beta + step;
    If[Max[Abs[step]] < 10.^-7, Break[]],
    {50}];
   beta];

poissonLinear[b_, x_, off_] := Clip[addIntercept[x] . Transpose[b] + off, {-30., 30.}];

poissonProbabilities[b_, x_, off_] := 1. - Exp[-Exp[poissonLinear[b, x, off]]];

poissonNLL[b_, x_, y_, off_] := With[{eta = poissonLinear[b, x, off]}, Total[Exp[eta] - y eta, 2]/Length[x]];

fitPoisson[xTr_, yTr_, offTr_, xVa_, yVa_, offVa_, grid_] := Module[{x1, k, candidates, scores, best},
   x1 = addIntercept[xTr];
   k = Dimensions[yTr][[2]];
   candidates = Table[Table[poissonFit[x1, yTr[[All, j]], offTr, lam], {j, k}], {lam, grid}];
   scores = Map[Function[b, poissonNLL[b, xVa, yVa, offVa]], candidates];
   best = First[Ordering[scores, 1]];
   <|"Coefficients" -> candidates[[best]], "Ridge" -> grid[[best]], "ValidationNLL" -> scores|>];

fitANN[xTr_, bTr_, xVa_, bVa_, c_] := Module[{p, k, h, net},
   p = Dimensions[xTr][[2]];
   k = Dimensions[bTr][[2]];
   h = c["Hidden"];
   net = NetChain[
     {LinearLayer[h], ElementwiseLayer[Ramp], DropoutLayer[0.2],
      LinearLayer[h], ElementwiseLayer[Ramp],
      LinearLayer[k], ElementwiseLayer[LogisticSigmoid]},
     "Input" -> p];
   NetTrain[net, Thread[xTr -> bTr],
    LossFunction -> CrossEntropyLossLayer["Binary"],
    ValidationSet -> Thread[xVa -> bVa],
    MaxTrainingRounds -> c["Epochs"],
    BatchSize -> 64,
    Method -> {"ADAM", "L2Regularization" -> 10.^-4},
    RandomSeeding -> c["Seed"],
    TrainingProgressReporting -> None,
    TrainingStoppingCriterion -> <|"Criterion" -> "Loss", "Patience" -> c["Patience"]|>]];

fitRNN[enc_, train_, xTr_, bTr_, val_, xVa_, bVa_, c_] := Module[{m, k, h, net, trainData, valData},
   m = Length[enc["SeqMeans"]];
   k = Dimensions[bTr][[2]];
   h = c["Hidden"];
   net = NetGraph[
     <|"gru" -> GatedRecurrentLayer[h],
      "last" -> SequenceLastLayer[],
      "join" -> CatenateLayer[],
      "d1" -> LinearLayer[h],
      "r1" -> ElementwiseLayer[Ramp],
      "d2" -> LinearLayer[k],
      "out" -> ElementwiseLayer[LogisticSigmoid]|>,
     {NetPort["Seq"] -> "gru" -> "last",
      {NetPort["Static"], "last"} -> "join",
      "join" -> "d1" -> "r1" -> "d2" -> "out"},
     "Seq" -> {"Varying", 2 m},
     "Static" -> Dimensions[xTr][[2]]];
   trainData = <|
     "Seq" -> Map[Function[s, encodeSeq[enc, s]], train[[All, "Sequence"]]],
     "Static" -> xTr,
     "Output" -> bTr|>;
   valData = <|
     "Seq" -> Map[Function[s, encodeSeq[enc, s]], val[[All, "Sequence"]]],
     "Static" -> xVa,
     "Output" -> bVa|>;
   NetTrain[net, trainData,
    LossFunction -> CrossEntropyLossLayer["Binary"],
    ValidationSet -> valData,
    MaxTrainingRounds -> c["Epochs"],
    BatchSize -> 32,
    Method -> {"ADAM", "L2Regularization" -> 10.^-4},
    RandomSeeding -> c["Seed"],
    TrainingProgressReporting -> None,
    TrainingStoppingCriterion -> <|"Criterion" -> "Loss", "Patience" -> c["Patience"]|>]];

clipProb[p_] := Clip[p, {10.^-6, 1. - 10.^-6}];

validProbabilities[m_] := MatrixQ[m, NumberQ];

componentProbabilities[ens_, profiles_, drug_ : Automatic] := Module[{enc, x, off, out, rnnOut},
   enc = ens["Encoder"];
   x = designMatrix[enc, profiles, drug];
   off = offsets[enc, profiles];
   out = <|
     "Poisson" -> clipProb[poissonProbabilities[ens["Poisson"]["Coefficients"], x, off]],
     "ANN" -> clipProb[N[ens["ANN"][x]]]|>;
   If[!MissingQ[ens["RNN"]],
    rnnOut = Quiet[Table[
       ens["RNN"][<|"Seq" -> encodeSeq[enc, profiles[[i, "Sequence"]]], "Static" -> x[[i]]|>],
       {i, Length[x]}]];
    If[validProbabilities[rnnOut], out = Append[out, "RNN" -> clipProb[N[rnnOut]]]]];
   out];

combine[ens_, comp_] := Total[MapThread[#1 #2 &, {Lookup[ens["Weights"], Keys[comp]], Values[comp]}]];

logLoss[p_, b_] := With[{q = clipProb[p]}, -Mean[Flatten[b Log[q] + (1. - b) Log[1. - q]]]];

auc[s_, l_] := Module[{pos = Pick[s, Thread[l > 0.5]], neg = Pick[s, Thread[l <= 0.5]]},
   If[Length[pos] == 0 || Length[neg] == 0,
    Missing[],
    (Total[Boole[Outer[Greater, pos, neg]], 2] + 0.5 Total[Boole[Outer[Equal, pos, neg]], 2])/(Length[pos] Length[neg])]];

macroAUC[p_, b_] := With[
   {values = DeleteMissing[Table[auc[p[[All, j]], b[[All, j]]], {j, Dimensions[b][[2]]}]]},
   If[values === {}, Missing[], Mean[values]]];

recallAtK[p_, b_, k_] := Module[{kk = Min[k, Dimensions[b][[2]]], values},
   values = DeleteMissing[Table[
      With[{pos = Flatten[Position[Thread[b[[i]] > 0.5], True]]},
       If[pos === {}, Missing[], Length[Intersection[pos, Ordering[p[[i]], -kk]]]/Length[pos]]],
      {i, Length[b]}]];
   If[values === {}, Missing[], N[Mean[values]]]];

metrics[p_, b_, k_] := <|"LogLoss" -> logLoss[p, b], "MacroAUC" -> macroAUC[p, b], "RecallAtK" -> recallAtK[p, b, k]|>;

chooseWeights[probs_Association, b_] := Module[{names = Keys[probs], mats = Values[probs], grid, losses, best},
   grid = Select[Tuples[Range[0, 10], Length[names]], Total[#] == 10 &]/10.;
   losses = Map[Function[w, logLoss[Total[MapThread[#1 #2 &, {w, mats}]], b]], grid];
   best = grid[[First[Ordering[losses, 1]]]];
   AssociationThread[names -> best]];

SEPIAFitEnsemble[profiles_List, split_Association, userConfig_Association : <||>] := Catch[Module[
    {c, train, val, vocab, enc, xTr, xVa, yTr, yVa, bTr, bVa, offTr, offVa, poisson, ann, rnn, ens, valProbs, weights},
    c = Join[SEPIADefaultConfig, userConfig];
    train = SEPIASubset[profiles, split["Train"]];
    val = SEPIASubset[profiles, split["Validation"]];
    If[train === {} || val === {},
     Throw[failure["EmptySplit", "Training and validation sets must both be non-empty."]]];
    vocab = vocabulary[train, c];
    If[vocab === {},
     Throw[failure["NoSideEffects", "No adverse event term reaches the minimum subject count in the training set."]]];
    SeedRandom[c["Seed"]];
    enc = fitEncoder[train, c];
    xTr = designMatrix[enc, train];
    xVa = designMatrix[enc, val];
    yTr = countMatrix[train, vocab];
    yVa = countMatrix[val, vocab];
    bTr = binaryMatrix[train, vocab];
    bVa = binaryMatrix[val, vocab];
    offTr = offsets[enc, train];
    offVa = offsets[enc, val];
    poisson = fitPoisson[xTr, yTr, offTr, xVa, yVa, offVa, c["RidgeGrid"]];
    ann = fitANN[xTr, bTr, xVa, bVa, c];
    rnn = If[c["SequenceFeatures"] =!= {} && AnyTrue[train[[All, "Sequence"]], (# =!= {}) &],
      With[{net = Quiet[fitRNN[enc, train, xTr, bTr, val, xVa, bVa, c]]},
       If[Head[net] === NetGraph, net, Missing["TrainingFailed"]]],
      Missing["NotTrained"]];
    ens = <|"Config" -> c, "Vocabulary" -> vocab, "Encoder" -> enc, "Poisson" -> poisson,
      "ANN" -> ann, "RNN" -> rnn, "Prevalence" -> Mean[bTr]|>;
    valProbs = componentProbabilities[ens, val];
    weights = chooseWeights[valProbs, bVa];
    Join[ens, <|"Weights" -> weights,
      "ValidationLogLoss" -> Map[Function[p, logLoss[p, bVa]], valProbs],
      "RNNStatus" -> If[MissingQ[rnn], rnn[[1]], If[KeyExistsQ[valProbs, "RNN"], "Used", "PredictionFailed"]]|>]]];

SEPIAEvaluate[ens_Association, profiles_List] := Module[{k, b, comp, prev},
   k = ens["Config"]["RecallK"];
   b = binaryMatrix[profiles, ens["Vocabulary"]];
   comp = componentProbabilities[ens, profiles];
   prev = ConstantArray[clipProb[ens["Prevalence"]], Length[profiles]];
   Join[
    Map[Function[p, metrics[p, b, k]], comp],
    <|"Ensemble" -> metrics[combine[ens, comp], b, k],
     "PrevalenceBaseline" -> metrics[prev, b, k]|>]];

patientProfile[ens_, patient_] := Module[{c = ens["Config"], e},
   e = cleanNumber[Lookup[patient, "Exposure", Missing[]]];
   <|"ID" -> "query",
    "Drug" -> "",
    "Numeric" -> Map[Function[f, cleanNumber[Lookup[patient, f, Missing[]]]], c["NumericFeatures"]],
    "Categorical" -> Map[Function[f, toLabel[cleanValue[Lookup[patient, f, Missing[]]]]], c["CategoricalFeatures"]],
    "Exposure" -> If[NumberQ[e] && e > 0, e, ens["Encoder"]["ExposureRef"]],
    "Counts" -> <||>,
    "Sequence" -> Map[Function[step, Map[cleanNumber, step]], Lookup[patient, "Sequence", {}]]|>];

SEPIARankSideEffects[ens_Association, patient_Association, drug_String, n_Integer : 5] :=
  If[!MemberQ[ens["Encoder"]["DrugLevels"], drug],
   failure["UnknownDrug", "The drug label was not present in the training data."],
   Module[{prof, comp, p, top},
    prof = patientProfile[ens, patient];
    comp = componentProbabilities[ens, {prof}, drug];
    p = combine[ens, comp][[1]];
    top = Reverse[Ordering[p, -Min[n, Length[p]]]];
    MapIndexed[
     Function[{j, pos},
      <|"Rank" -> First[pos],
       "SideEffect" -> ens["Vocabulary"][[j]],
       "Probability" -> p[[j]],
       "Components" -> Map[Function[m, m[[1, j]]], comp]|>],
     top]]];

SEPIASaveEnsemble[ens_Association, dir_String] := (
   If[!DirectoryQ[dir], CreateDirectory[dir, CreateIntermediateDirectories -> True]];
   Export[FileNameJoin[{dir, "ann.wlnet"}], ens["ANN"], "WLNet"];
   If[!MissingQ[ens["RNN"]], Export[FileNameJoin[{dir, "rnn.wlnet"}], ens["RNN"], "WLNet"]];
   Export[FileNameJoin[{dir, "ensemble.wxf"}], KeyDrop[ens, {"ANN", "RNN"}], "WXF"];
   dir);

SEPIALoadEnsemble[dir_String] := Module[{base, rnnFile},
   base = Import[FileNameJoin[{dir, "ensemble.wxf"}], "WXF"];
   rnnFile = FileNameJoin[{dir, "rnn.wlnet"}];
   Join[base, <|
     "ANN" -> Import[FileNameJoin[{dir, "ann.wlnet"}], "WLNet"],
     "RNN" -> If[FileExistsQ[rnnFile], Import[rnnFile, "WLNet"], Missing["NotTrained"]]|>]];

SEPIARankFromJSON[dir_String, jsonFile_String, n_Integer : 5] := Module[{ens, request},
   ens = SEPIALoadEnsemble[dir];
   request = Import[jsonFile, "RawJSON"];
   ExportString[SEPIARankSideEffects[ens, Lookup[request, "patient", <||>], request["drug"], n], "RawJSON"]];

SEPIASyntheticData[n_Integer : 1500, seed_Integer : 7] := Module[
   {drugs, terms, base, ageEffect, drugEffect, subjects, lambda, counts, adsl, adae, seq},
   SeedRandom[seed];
   drugs = {"DrugA", "DrugB", "Placebo"};
   terms = {"NAUSEA", "FATIGUE", "ANAEMIA", "NEUTROPENIA", "DIARRHOEA", "ALT INCREASED", "RASH", "HEADACHE"};
   base = {-3.2, -3.5, -3.8, -4., -3.6, -4.2, -4.2, -3.4};
   ageEffect = {0., 0.2, 0.3, 0.1, 0., 0.1, -0.1, -0.3};
   drugEffect = <|
     "DrugA" -> {0.8, 0.5, 0.1, 0.9, 0.6, 0.1, 0., 0.},
     "DrugB" -> {0., 0.2, 0.9, 0.2, 0., 0.9, 0.3, 0.1},
     "Placebo" -> ConstantArray[0., 8]|>;
   subjects = Table[
     <|"ID" -> "S" <> IntegerString[i, 10, 5],
      "Drug" -> RandomChoice[drugs],
      "Age" -> RandomVariate[NormalDistribution[60, 10]],
      "Sex" -> RandomChoice[{"M", "F"}],
      "Race" -> RandomChoice[{0.6, 0.25, 0.15} -> {"WHITE", "ASIAN", "OTHER"}],
      "Duration" -> RandomInteger[{30, 180}],
      "LogAlt" -> RandomVariate[NormalDistribution[3.4, 0.4]]|>,
     {i, n}];
   lambda = Map[
     Function[s,
      Exp[base + ageEffect (s["Age"] - 60)/10 + drugEffect[s["Drug"]] + UnitVector[8, 6] 0.9 (s["LogAlt"] - 3.4)/0.4] s["Duration"]/30],
     subjects];
   counts = Map[Function[lam, Map[Function[l, RandomVariate[PoissonDistribution[l]]], lam]], lambda];
   adsl = Map[
     Function[s,
      <|"USUBJID" -> s["ID"],
       "TRT01A" -> s["Drug"],
       "AGE" -> If[RandomReal[] < 0.05, Missing[], Round[s["Age"]]],
       "SEX" -> s["Sex"],
       "RACE" -> s["Race"],
       "TRTDURD" -> s["Duration"]|>],
     subjects];
   adae = Flatten[Table[
      ConstantArray[<|"USUBJID" -> subjects[[i, "ID"]], "AEDECOD" -> terms[[j]]|>, counts[[i, j]]],
      {i, n}, {j, 8}]];
   seq = Flatten[Map[
      Function[s,
       Table[
        <|"USUBJID" -> s["ID"],
         "ADY" -> d,
         "ALT" -> Exp[s["LogAlt"] + RandomVariate[NormalDistribution[0, 0.1]]],
         "CREAT" -> RandomVariate[NormalDistribution[0.9, 0.15]]|>,
        {d, {-14, -7, 0}}]],
      subjects]];
   <|"ADSL" -> adsl, "ADAE" -> adae, "SEQ" -> seq|>];

End[];

EndPackage[];
