module AllTests(main) where

import Test.HUnit

import SearchTests
import TacticProveTests

main = do
  runTestTT $ TestList [allSearchTests, allTacticProveTests]
