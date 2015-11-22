module Search(tryToProve) where

import Data.List as L
import Data.Maybe

import Proof

maxDepth = 7

tryToProve :: Conjecture -> Maybe Proof
tryToProve c = dfs actions c maxDepth

tryToProve' :: (Conjecture -> Maybe Action) -> Conjecture -> Maybe Proof
tryToProve' s c =
  case s c of
   Just a -> applyAction s a c
   Nothing -> Nothing

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

applyAction s a c =
  case (acApplies a) c of
   True ->
     let subgoals = (acGenSubgoals a) c
         results = catMaybes $ L.map (tryToProve' s) subgoals in
      case allProved subgoals results of
       True -> Just $ (acGenProof a) c results
       False -> Nothing
