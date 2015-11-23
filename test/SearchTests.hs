module SearchTests(allSearchTests) where

import Data.List as L
import Test.HUnit

import Boolean
import Core
import Proof
import Nat
import Search
import TestUtils
import Utils

allSearchTests =
  TestList $ L.map (\(t, r) -> testThm t r) $ thms ++ nonThms

thms = L.map (\t -> (t, True)) $ boolThms ++ natThms

nonThms = L.map (\t -> (t, False)) boolNonThms

testThm thm expected = TestCase $ tf thm expected

tf thm expected = do
  let p = someProof $ tryToProve thm in
   assertBool (caseFailMsg thm p expected) (p == expected)

caseFailMsg thm p expected =
  "Input:\n" ++ pretty 0 thm ++ "\nResult: " ++ show p

someProof res =
  case res of
   Just _ -> True
   Nothing -> False
