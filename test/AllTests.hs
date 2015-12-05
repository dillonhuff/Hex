module AllTests(allTests) where

import Test.HUnit

import LiftingTests
import TacticProveTests
allTests = TestList [allTacticProveTests,
                     allLiftingTests]
