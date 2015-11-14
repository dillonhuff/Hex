module ProofTests(allProofTests) where

import Test.HUnit

import Boolean
import Proof
import TestUtils

allProofTests =
  TestList [testThm trueIsTrue]

trueIsTrue = conjecture [boolDT] [] trueTerm

testThm thm = TestCase $ tf thm

tf thm = do
  let p = someProof $ tryToProve thm in
   assertBool (caseFailMsg thm p) p

caseFailMsg thm p = "Input: " ++ show thm ++ "\nResult: " ++ show p

someProof res =
  case res of
   Just _ -> True
   Nothing -> False

caseFile name = testPath ++ name ++ ".smt2"

testPath = "/Users/dillon/Haskell/ThmProving/Hex/test/cases/"
