module Search(tryToProve,
              dfs, dfsAction) where

import Data.List as L
import Data.Maybe

import Action
import BasicActions
import Proof

maxDepth = 7

tryToProve :: Conjecture -> Maybe Proof
tryToProve c = dfs basicActions c maxDepth

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

dfsAction :: [Action] -> Int -> Action
dfsAction acs d =
  action $ (\c -> dfsAc acs c d)
  
dfsAc :: [Action] -> Conjecture -> Int -> Maybe ([Conjecture], [Proof] -> Proof)
dfsAc _ _ 0 = Nothing
dfsAc [] c d = Nothing
dfsAc as c d =
  let results = L.map (\a -> dfsAcCall a as c (d - 1)) as
      successes = L.filter isJust results in
   case L.length successes == 0 of
    True -> Nothing
    False -> L.head successes

dfsAcCall :: Action -> [Action] -> Conjecture -> Int -> Maybe ([Conjecture], [Proof] -> Proof)
dfsAcCall a as c d =
  case (acApplies a) c of
   Just ([], genProof) -> Just ([], genProof)
   Just (subgoals, genProof) ->
     let results = L.map (\s -> dfsAc as s d) subgoals
         potentialSuccesses = catMaybes results
         successes = L.filter (\(sgs, _) -> sgs == []) potentialSuccesses in
      case (L.length successes) /= (L.length subgoals) of
       True -> Nothing
       False ->
         let subproofs = L.map (\(_, pfunc) -> pfunc []) successes in
          Just ([], \[] -> genProof subproofs)
   Nothing -> Nothing
