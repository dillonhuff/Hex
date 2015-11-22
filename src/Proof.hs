module Proof(Proof,
             Action,
             Conjecture,
             acGenProof, acGenSubgoals, acApplies,
             actions,
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
  pretty n (EqProof c) = (indent n $ pretty n c) ++ (indent n "@@@ ASSUMPTION @@@")
  pretty n (UnfoldProof c p) =
    (indent n $ pretty n c) ++ (indent n "@@@ UNFOLD @@@") ++ (indent n $ pretty n p)
  pretty n (SelectProof c p) = (indent n $ pretty n c) ++ (indent n "@@@ SELECT @@@") ++ (pretty n p)
  pretty n (SplitVarProof c ps) = (indent n $ pretty n c) ++ (indent n "@@@ SPLIT @@@") ++ (L.concatMap (pretty (n+1)) ps)
  pretty n (InductionProof c ps) = (indent n $ pretty n c) ++ (indent n "@@@ INDUCTION @@@") ++ (L.concatMap (pretty (n+1)) ps)

trueProof = TrueProof
eqProof = EqProof
unfoldProof = UnfoldProof
selectProof = SelectProof
splitVarProof = SplitVarProof
inductionProof = InductionProof

actions = [eqAction,
           selectMatchAction,
           unfoldAction,
           splitLocalAction,
           inductionAction]

data Action
  = Action {
    acApplies :: Conjecture -> Bool,
    acGenSubgoals :: Conjecture -> [Conjecture],
    acGenProof :: Conjecture -> [Proof] -> Proof
    }

eqAction =
  Action assumeEq (\_ -> []) (\c _ -> eqProof c)
unfoldAction =
  Action existsFunc substituteFunc simpleUnfoldProof
selectMatchAction =
  Action existsDataConMatch substituteDataConMatches simpleSelectProof
splitLocalAction =
  Action existsLcl splitLcl splitVarProof
inductionAction =
  Action existsLcl inductionLcl inductionProof

assumeEq c =
  let t0 = fst $ conjAssert c
      t1 = snd $ conjAssert c in
   t0 == t1 || (L.filter (\(a0, a1) -> (t0 == a0 && t1 == a1)) $ conjAssumptions c) /= []

existsLcl c =
  (collectLcls $ fst $ conjAssert c) /= []

inductionLcl c =
  let lclToInd = L.head $ collectLcls $ fst $ conjAssert c
      cons = lookupConstructors (lclType $ getLocal lclToInd) c in
   L.map (inductionSubgoal lclToInd c) cons

inductionSubgoal l c con =
  let k = freshConstructorCall (fst $ conjAssert c) con
      fvs = callArgs k
      recVars = L.filter (\v -> (lclType $ getLocal v) == (lclType $ getLocal l)) fvs
      oldAssert = fst $ conjAssert c
      newAssert = genSub (\t -> t == l) (\t -> k) oldAssert
      newAssumptions = L.map (\x -> (genSub (\t -> t == l) (\t -> x) oldAssert, snd $ conjAssert c)) fvs in
   c { conjAssumptions = newAssumptions ++ (conjAssumptions c),
       conjAssert = (newAssert, snd $ conjAssert c) }      
  
splitLcl c =
  let lclToSplit = L.head $ collectLcls $ fst $ conjAssert c
      cons = lookupConstructors (lclType $ getLocal lclToSplit) c in
   L.map (addSplitAssumption lclToSplit c) cons

addSplitAssumption lcl c con =
  c { conjAssert = (genSub (\t -> t == lcl) (\t -> freshConstructorCall (fst $ conjAssert c) con) (fst $ conjAssert c), snd $ conjAssert c) }

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

existsDataConMatch c = existsTerm (isDataConMatch c) $ fst $ conjAssert c
substituteDataConMatches c =
  [c {conjAssert = (genSub (isDataConMatch c) selectMatch $ fst $ conjAssert c, snd $ conjAssert c)}]

existsFunc c =
  existsTerm (isFuncall c) $ fst $ conjAssert c

substituteFunc c =
  case conjFunctions c of
   (f:fs) ->
      [c { conjAssert = (replaceFuncWithBody f $ fst $ conjAssert c, snd $ conjAssert c)}]
   _ -> [c]

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
