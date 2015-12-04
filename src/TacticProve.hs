module TacticProve(tacticProve,
                   evaluate,
                   mpSplitAction) where

import Data.List as L
import Data.Maybe

import Action
import BasicActions
import Core
import Lift
import Proof
import Search
import Utils

maxDepth = 8

tacticProve :: Conjecture -> Maybe Proof
tacticProve c = dfs tactics c maxDepth

repeatAc :: Action -> Action
repeatAc a =
  action $ (\c -> repAc (acApplies a) c)

repAc a c =
  case a c of
   Nothing -> Nothing
   Just ([], pf) -> Just ([], pf)
   Just ([sg], pf) -> do
     ([], pf2) <- repAc a sg
     return $ ([], \[] -> pf [pf2 []])

tactics = [eqAction,
           evaluate,
           selectMatchAction,
           unfoldAction,
           mpSplitAction,
           splitLocalAction,
           substActionLHS,
           substActionRHS,
           symmetryAction,
           inductionAction]

evaluate =
  repeatAc $ applyIf noFreeVarsInAssert $ applyFirst [eqAction, selectMatchAction, unfoldAction]

noFreeVarsInAssert c =
  noFreeVars (fst $ conjAssert c) && noFreeVars (snd $ conjAssert c)

mpSplitAction =
  action mpSplit

mpSplit c =
   case rewrittenSplits c of
    [] -> Nothing
    newAssumptions ->
      let newAssume = L.head newAssumptions
          nh = c { conjAssumptions = (conjAssumptions c) ++ newAssume }
          subgoals = nh : (L.map (\na -> c { conjAssert = na}) newAssume) in
      Just (subgoals, \pfs -> modusPonensProof c pfs)

rewrittenSplits :: Conjecture -> [[(Term, Term)]]
rewrittenSplits c =
  let as = conjAssumptions c
      a = conjAssert c in
   catMaybes $ L.map (findRewritesTo a) as

-- NOTE: findRewrites to should actually be something like
-- "find a rewrite of 'from' such that 'to' is a subterm of
-- the rewrite"
findRewritesTo :: (Term, Term) -> (Term, Term) -> Maybe [(Term, Term)]
findRewritesTo from to = do
  lr <- liftTerm (fst to) (fst from)
  rr <- liftTerm (snd to) (snd from)
--  error $ pretty 0 (fst from) ++ "\t" ++ pretty 0 lr ++ "\n" ++ pretty 0 rr
  return [(fst from, lr), (snd from, rr)]

unfoldSelectAction = applySequence [unfoldAction, selectMatchAction]
