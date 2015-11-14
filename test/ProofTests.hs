module ProofTests(allProofTests) where

import Test.HUnit

import Proof
import TestUtils

allProofTests =
  TestList [testFile "bool_t"]

testFile n = TestCase $ tf n

tf n = do
  res <- proveFile (caseFile n)
  assertBool (caseFailMsg n) (someProof res)

caseFailMsg n = n

someProof res =
  case res of
   Just _ -> True
   Nothing -> False

caseFile name = testPath ++ name ++ ".smt2"

testPath = "/Users/dillon/Haskell/ThmProving/Hex/test/cases/"
