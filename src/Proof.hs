module Proof(tryToProve) where

import Data.List as L
import Data.Maybe

import Core

data Proof
  = TrueProof Conjecture
  | UnfoldProof Conjecture Proof
    deriving (Eq, Ord, Show)

trueProof = TrueProof
unfoldProof = UnfoldProof

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
           simpleUnfoldAction]

data Action
  = Action {
    acApplies :: Conjecture -> Bool,
    acGenSubgoals :: Conjecture -> [Conjecture],
    acGenProof :: Conjecture -> [Proof] -> Proof
    }

eqAction = Action eqTerm (\_ -> []) (\c _ -> trueProof c)

simpleUnfoldAction =
  Action existsNoArgFunc substituteFirstNoArgFunc simpleUnfoldProof

existsNoArgFunc c =
  case firstNoArgFunc $ conjFunctions c of
   Just f -> True
   Nothing -> False

substituteFirstNoArgFunc c =
  case firstNoArgFunc $ conjFunctions c of
   Just f ->
      [c {conjFunctions = L.delete f $ conjFunctions c,
          conjAssert = replaceFuncWithBody f $ conjAssert c}]
   Nothing -> [c]
   
simpleUnfoldProof c [subProof] = unfoldProof c subProof

replaceFuncWithBody f =
  let fName = funcName f
      fBody = funcBody f in
   genSub (sameFunc fName) (replaceFunc fBody)

firstNoArgFunc [] = Nothing
firstNoArgFunc (f:fs) =
  if funcArgs f == [] then Just f else firstNoArgFunc fs
