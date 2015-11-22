module Search(tryToProve) where

import Data.List as L
import Data.Maybe

import Action
import Proof

maxDepth = 7

tryToProve :: Conjecture -> Maybe Proof
tryToProve c = dfs actions c maxDepth

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
   Just (subgoals, genProof) ->
     let results = L.map (\s -> dfs as s d) subgoals
         successes = catMaybes results in
      case (L.length successes) /= (L.length subgoals) of
       True -> Nothing
       False -> Just $ genProof successes
   Nothing -> Nothing

allProved subgoals results =
  L.length subgoals == L.length results
