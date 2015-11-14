module ProofTests(allProofTests) where

import Test.HUnit

import Proof
import TestUtils

allProofTests =
  TestList [testFile "bool_t"]

testFile n = TestCase $ tf n

tf n = do
  res <- proveFile (caseFile n)
  let p = someProof res in
   assertBool (caseFailMsg n p) p

caseFailMsg n p = "Input: " ++ n ++ "\nResult: " ++ show p

someProof res =
  case res of
   Just _ -> True
   Nothing -> False

caseFile name = testPath ++ name ++ ".smt2"

testPath = "/Users/dillon/Haskell/ThmProving/Hex/test/cases/"
