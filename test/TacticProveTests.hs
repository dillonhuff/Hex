module TacticProveTests(allTacticProveTests) where

import Data.List as L
import Data.Maybe
import Test.HUnit

import Action
import BasicActions
import Boolean
import Core
import Proof
import Nat
import Search
import TacticProve
import TestUtils
import Utils

allTacticProveTests =
  TestList [TestList $ L.map (\(t, r) -> testThm tacticProve t r) $ thms ++ nonThms,
            TestList $ L.map (\(t, r) -> testThm tacticProve t r) $ evalThms]

thms = L.map (\t -> (t, True)) $ boolThms ++ natThms

nonThms = L.map (\t -> (t, False)) boolNonThms

evalThms =
  L.map (\t -> (t, True))
  [trueIsTrue,
   trueFuncIsTrue,
   beqFalseFalse]

pp t = putStrLn $ pretty 0 $ t
pProof thm = putStrLn $ pretty 0 $ fromJust $ tacticProve thm
pThm thm = putStrLn $ pretty 0 $ thm
apAc a thm = putStrLn $ pretty 0 $ fst $ fromJust $ (acApplies a) thm
sgs a thm = fst $ fromJust $ (acApplies a) thm

commSgs = sgs mpSplitAction $ (sgs inductionAction natplusComm) !! 1

commSgs2 = (commSgs !! 2) { conjAssumptions = [] }

gs a b c =
  c { conjAssert = (genSub (\t -> t == nv a) (\t -> nv b) $ fst $ conjAssert c,
                    genSub (\t -> t == nv a) (\t -> nv b) $ snd $ conjAssert c) }

newGoal = gs "$0" "b" $ gs "b" "a" commSgs2

gotProof (Just a) = "Proved"
gotProof Nothing = "Nothing"
