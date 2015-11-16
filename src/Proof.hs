module Proof(tryToProve, substituteFunc) where

import Data.List as L
import Data.Maybe

import Core

data Proof
  = TrueProof Conjecture
  | UnfoldProof Conjecture Proof
  | SelectProof Conjecture Proof
    deriving (Eq, Ord, Show)

trueProof = TrueProof
unfoldProof = UnfoldProof
selectProof = SelectProof

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
           selectMatchAction]

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
