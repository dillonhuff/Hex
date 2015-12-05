module LiftingTests(allLiftingTests) where

import Data.List as L
import Test.HUnit

import Lift
import Nat
import TestUtils

allLiftingTests =
  TestList $ L.map testLift liftCases

testLift (input, expected) =
  testCasePP (\(a, b) -> liftTerm a b) input expected

liftCases =
  [((natp (nv "a") (s $ nv "b"), natp (s $ nv "a") (s $ nv "b")),
    Just $ s $ natp (nv "a") (s $ nv "b")),
   ((s $ nv "a", z), Nothing),
   ((s $ natp (nv "a") (nv "b"), s $ natp (s $ nv "a") (nv "b")), Just $ s $ s $ natp (nv "a") (nv "b")),
   ((nv "a", nv "a"), Just $ nv "a"),
   ((natp (natp (nv "a") (nv "b")) (nv "c"), natp (natp (s $ nv "a") (nv "b")) (nv "c")), Just $ s $ natp (natp (nv "a") (nv "b")) (nv "c"))]
