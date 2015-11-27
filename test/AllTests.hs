module AllTests(main) where

import Test.HUnit

import TacticProveTests

main = do
  runTestTT $ TestList [allTacticProveTests]
