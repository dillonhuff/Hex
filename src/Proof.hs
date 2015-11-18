module Proof(tryToProve,
             inductionLcl,
             substituteFunc) where

import Data.List as L
import Data.Maybe

import Core
import Utils

data Proof
  = TrueProof Conjecture
  | EqProof Conjecture
  | UnfoldProof Conjecture Proof
  | SelectProof Conjecture Proof
  | SplitVarProof Conjecture [Proof]
  | InductionProof Conjecture [Proof]
    deriving (Eq, Ord, Show)

instance Pretty Proof where
  pretty n (TrueProof c) = (indent n $ pretty n c) ++ (indent n "@@@ TRUE @@@")
  pretty n (UnfoldProof c p) =
    (indent n $ pretty n c) ++ (indent n "@@@ UNFOLD @@@") ++ (indent n $ pretty n p)
  pretty n (SelectProof c p) = (indent n $ pretty n c) ++ (indent n "@@@ SELECT @@@") ++ (pretty n p)
  pretty n (SplitVarProof c ps) = (indent n $ pretty n c) ++ (indent n "@@@ SPLIT @@@") ++ (L.concatMap (pretty (n+1)) ps)
  
trueProof = TrueProof
eqProof = EqProof
unfoldProof = UnfoldProof
selectProof = SelectProof
splitVarProof = SplitVarProof
inductionProof = InductionProof

maxDepth = 7

tryToProve :: Conjecture -> Maybe Proof
tryToProve c = dfs actions c maxDepth --tryToProve' selectAction c

tryToProve' :: (Conjecture -> Maybe Action) -> Conjecture -> Maybe Proof
tryToProve' s c =
  case s c of
   Just a -> applyAction s a c
   Nothing -> Nothing

applyAction s a c =
  case (acApplies a) c of
   True ->
     let subgoals = (acGenSubgoals a) c
         results = catMaybes $ L.map (tryToProve' s) subgoals in
      case allProved subgoals results of
       True -> Just $ (acGenProof a) c results
       False -> Nothing

dfs :: [Action] -> Conjecture -> Int -> Maybe Proof
dfs _ _ 0 = Nothing
dfs [] c d = Nothing
dfs as c d =
  let results = L.map (\a -> dfsCall a as c (d - 1)) as
      successes = L.filter isJust results in
   case successes == [] of
    True -> Nothing
    False -> L.head successes

dfsCall :: Action -> [Action] -> Conjecture -> Int -> Maybe Proof
dfsCall a as c d =
  case (acApplies a) c of
   True ->
     let subgoals = (acGenSubgoals a) c
         results = L.map (\s -> dfs as s d) subgoals
         successes = catMaybes results in
      case (L.length successes) /= (L.length subgoals) of
       True -> Nothing
       False -> Just $ (acGenProof a) c successes
   False -> Nothing

allProved subgoals results =
  L.length subgoals == L.length results

selectAction c = selectAction' c actions

selectAction' _ [] = Nothing
selectAction' c (a:as) =
  case (acApplies a) c of
   True -> Just a
   False -> selectAction' c as

actions = [inductionAction,
           eqAction,
           unfoldAction,
           selectMatchAction,
           splitLocalAction,
           eqAction2]

data Action
  = Action {
    acApplies :: Conjecture -> Bool,
    acGenSubgoals :: Conjecture -> [Conjecture],
    acGenProof :: Conjecture -> [Proof] -> Proof
    }

eqAction2 =
  Action assumeEq (\_ -> []) (\c _ -> eqProof c)
eqAction =
  Action eqTerm (\_ -> []) (\c _ -> trueProof c)
unfoldAction =
  Action existsFunc substituteFunc simpleUnfoldProof
selectMatchAction =
  Action existsDataConMatch substituteDataConMatches simpleSelectProof
splitLocalAction =
  Action existsLcl splitLcl splitVarProof
inductionAction =
  Action existsLcl inductionLcl inductionProof

assumeEq c =
  let g = conjAssert c in
   (L.filter (\(t1, t2) -> (t1 == g && t2 == trueTermC)) $ conjAssumptions c) /= []

existsLcl c =
  (collectLcls $ conjAssert c) /= []

inductionLcl c =
  let lclToInd = L.head $ collectLcls $ conjAssert c
      cons = lookupConstructors (lclType $ getLocal lclToInd) c in
   L.map (inductionSubgoal lclToInd c) cons

inductionSubgoal l c con =
  let k = freshConstructorCall (conjAssert c) con
      fvs = callArgs k
      recVars = L.filter (\v -> (lclType $ getLocal v) == (lclType $ getLocal l)) fvs
      oldAssert = conjAssert c
      newAssert = genSub (\t -> t == l) (\t -> k) oldAssert
      newAssumptions = L.map (\x -> (genSub (\t -> t == l) (\t -> x) oldAssert, trueTermC)) fvs in
   c { conjAssumptions = newAssumptions ++ (conjAssumptions c),
       conjAssert = newAssert }
      
  
splitLcl c =
  let lclToSplit = L.head $ collectLcls $ conjAssert c
      cons = lookupConstructors (lclType $ getLocal lclToSplit) c in
   L.map (addSplitAssumption lclToSplit c) cons

addSplitAssumption lcl c con =
  c { conjAssert = genSub (\t -> t == lcl) (\t -> freshConstructorCall (conjAssert c) con) (conjAssert c) }

freshConstructorCall t con =
  let fvs = freshVars t (dcArgs con) in
   ap (dcName con) fvs

freshVars t args =
  let n = nextTempIndex t in
   L.zipWith (\t i -> lcl $ dLcl (tempVarName i) t) args [n..((L.length args) - 1)]

nextTempIndex t =
  let tmps = collectFromTerms (\t -> if isTemp t then [t] else []) t
      tmpVals = L.map (\tmp -> read $ L.drop 1 $ idName $ lclName $ getLocal tmp) tmps in
   if tmpVals == [] then 0 else (L.maximum tmpVals) + 1

isTemp t = isLcl t && (L.head $ idName $ lclName $ getLocal t) == '$'

tempVarName i = "$" ++ show i

lookupConstructors t c =
  let tName = tyConName t
      dts = conjDataTypes c in
   dtConstructors $ L.head $ L.filter (\dt -> dtName dt == tName) dts

collectLcls tm = collectFromTerms (\t -> if isLcl t then [t] else []) tm

existsDataConMatch c = existsTerm (isDataConMatch c) $ conjAssert c
substituteDataConMatches c =
  [c {conjAssert = genSub (isDataConMatch c) selectMatch $ conjAssert c}]

existsFunc c =
  existsTerm (isFuncall c) $ conjAssert c

substituteFunc c =
  case conjFunctions c of
   (f:fs) ->
      [c { conjAssert = replaceFuncWithBody f $ conjAssert c}]
   _ -> [c]

-- conjFunctions = L.delete f $ conjFunctions c,

simpleUnfoldProof c [subProof] = unfoldProof c subProof
simpleSelectProof c [subProof] = selectProof c subProof

replaceFuncWithBody f =
  let fName = funcName f
      fBody = funcBody f
      fArgs = funcArgs f in
   genSub (sameFunc fName) (replaceFunc fBody fArgs)

firstNoArgFunc [] = Nothing
firstNoArgFunc (f:fs) =
  if funcArgs f == [] then Just f else firstNoArgFunc fs
