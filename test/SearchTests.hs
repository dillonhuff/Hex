module SearchTests(allSearchTests) where

import Data.List as L
import Test.HUnit

import Boolean
import Core
import Proof
import Nat
import Search
import TacticProve
import TestUtils
import Utils

allSearchTests =
  TestList $ L.map (\(t, r) -> testThm tryToProve t r) $ thms ++ nonThms

thms = L.map (\t -> (t, True)) $ boolThms ++ natThms

nonThms = L.map (\t -> (t, False)) boolNonThms
