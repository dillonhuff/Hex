module TacticProveTests(allTacticProveTests) where

import Data.List as L
import Test.HUnit

import Action
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
