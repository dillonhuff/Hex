module Proof(tryToProve, substituteFunc) where

import Data.List as L
import Data.Maybe

import Core
import Utils

data Proof
  = TrueProof Conjecture
  | UnfoldProof Conjecture Proof
  | SelectProof Conjecture Proof
  | SplitVarProof Conjecture [Proof]
    deriving (Eq, Ord, Show)

instance Pretty Proof where
  pretty n (TrueProof c) = (indent n $ pretty n c) ++ (indent n "@@@ TRUE @@@")
  pretty n (UnfoldProof c p) =
    (indent n $ pretty n c) ++ (indent n "@@@ UNFOLD @@@") ++ (indent n $ pretty n p)
  pretty n (SelectProof c p) = (indent n $ pretty n c) ++ (indent n "@@@ SELECT @@@") ++ (pretty n p)
  
trueProof = TrueProof
unfoldProof = UnfoldProof
selectProof = SelectProof
splitVarProof = SplitVarProof

tryToProve :: Conjecture -> Maybe Proof
tryToProve c = tryToProve' selectAction c

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

allProved subgoals results =
  L.length subgoals == L.length results

selectAction c = selectAction' c actions

selectAction' _ [] = Nothing
selectAction' c (a:as) =
  case (acApplies a) c of
   True -> Just a
   False -> selectAction' c as

actions = [eqAction,
           unfoldAction,
           selectMatchAction,
           splitLocalAction]

data Action
  = Action {
    acApplies :: Conjecture -> Bool,
    acGenSubgoals :: Conjecture -> [Conjecture],
    acGenProof :: Conjecture -> [Proof] -> Proof
    }

eqAction =
  Action eqTerm (\_ -> []) (\c _ -> trueProof c)

unfoldAction =
  Action existsFunc substituteFunc simpleUnfoldProof
selectMatchAction =
  Action existsDataConMatch substituteDataConMatches simpleSelectProof
splitLocalAction =
  Action existsLcl splitLcl splitVarProof

existsLcl c =
  (collectLcls $ conjAssert c) /= []

splitLcl c =
  let lclToSplit = L.head $ collectLcls $ conjAssert c
      cons = lookupConstructors (lclType $ getLocal lclToSplit) c in
   L.map (addSplitAssumption lclToSplit c) cons

addSplitAssumption lcl c con =
  c { conjAssert = genSub (\t -> t == lcl) (\t -> freshConstructorCall (conjAssert c) con) (conjAssert c) }

freshConstructorCall t con =
  let fvs = freshVars t (dcArgs con) in
   ap (dcName con) fvs

freshVars t [] = []

lookupConstructors t c =
  let tName = tyConName t
      dts = conjDataTypes c in
   dtConstructors $ L.head $ L.filter (\dt -> dtName dt == tName) dts

collectLcls tm = collectFromTerms (\t -> if isLcl t then [t] else []) tm

existsDataConMatch c = existsTerm (isDataConMatch c) $ conjAssert c
substituteDataConMatches c =
  [c {conjAssert = genSub (isDataConMatch c) selectMatch $ conjAssert c}]

existsFunc c =
  case conjFunctions c of
   [] -> False
   _ -> True

substituteFunc c =
  case conjFunctions c of
   (f:fs) ->
      [c {conjFunctions = L.delete f $ conjFunctions c,
          conjAssert = replaceFuncWithBody f $ conjAssert c}]
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
