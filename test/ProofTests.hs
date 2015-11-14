module ProofTests(allProofTests) where

import Test.HUnit

import Boolean
import Proof
import TestUtils

allProofTests =
  TestList [testThm trueIsTrue True,
            testThm trueFuncIsTrue True]

trueIsTrue = conjecture [boolDT] [] trueTerm

trueFuncIsTrue = conjecture [boolDT] [trueFunc] trueFuncCall

trueFunc = function (dId "trueFunc") [] [] boolType trueTerm

trueFuncCall = ap (dGbl "trueFunc" (func [] boolType) []) []

testThm thm expected = TestCase $ tf thm expected

tf thm expected = do
  let p = someProof $ tryToProve thm in
   assertBool (caseFailMsg thm p expected) (p == expected)

caseFailMsg thm p expected =
  "Input: " ++ show thm ++ "\nResult: " ++ show p

someProof res =
  case res of
   Just _ -> True
   Nothing -> False

caseFile name = testPath ++ name ++ ".smt2"

testPath = "/Users/dillon/Haskell/ThmProving/Hex/test/cases/"
