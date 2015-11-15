module ProofTests(allProofTests) where

import Test.HUnit

import Boolean
import Core
import Proof
import TestUtils

allProofTests =
  TestList [testThm trueIsTrue True,
            testThm trueFuncIsTrue True,
            testThm falseIsTrue False,
            testThm falseFuncIsTrue False]

trueIsTrue = conjecture [boolDT] [] trueTerm

trueFuncIsTrue = conjecture [boolDT] [trueFunc] trueFuncall

falseIsTrue = conjecture [boolDT] [] falseTerm

falseFuncIsTrue = conjecture [boolDT] [falseFunc] falseFuncall

trueFunc = function (dId "trueFunc") [] [] boolType trueTerm
falseFunc = function (dId "falseFunc") [] [] boolType falseTerm

trueFuncall = ap (dGbl "trueFunc" (func [] boolType) []) []
falseFuncall = ap (dGbl "falseFunc" (func [] boolType) []) []

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
