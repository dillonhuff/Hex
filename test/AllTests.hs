module AllTests(main) where

import Test.HUnit

import ProofTests

main = do
  runTestTT allProofTests
