module AllTests(main) where

import Test.HUnit

import LiftingTests
import TacticProveTests

main = do
  runTestTT $ TestList [allTacticProveTests,
                        allLiftingTests]
