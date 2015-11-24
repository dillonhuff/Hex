module TacticProve(tacticProve,
                   evaluate) where

import Action
import BasicActions
import Core
import Proof
import Search

maxDepth = 3

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

tactics = [evaluate]

evaluate =
  repeatAc $ applyIf noFreeVarsInAssert $ applyFirst [eqAction, selectMatchAction, unfoldAction]

noFreeVarsInAssert c =
  noFreeVars (fst $ conjAssert c) && noFreeVars (snd $ conjAssert c)
