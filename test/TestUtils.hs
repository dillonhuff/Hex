module TestUtils(testFunction,
                 testThm,
                 testCase, testCasePP) where

import Test.HUnit

import Utils

testFunction func cases = runTestTT $ makeTestCases func cases

makeTestCases func cases =
  TestList $ map (\(input, expected) -> testCase func input expected) cases

testCasePP func input expected =
  TestCase (assertEqual ("Input:\n" ++ (pretty 0 input) ++ "\nWanted\n" ++ (pretty 0 expected)) expected (func input))

testCase func input expected =
  TestCase (assertEqual ("Input: " ++ (show input)) expected (func input))

testThm p thm expected = TestCase $ tf p thm expected

tf f thm expected = do
  let p = someProof $ f thm in
   assertBool (caseFailMsg thm p expected) (p == expected)

caseFailMsg thm p expected =
  "Input:\n" ++ pretty 0 thm ++ "\nResult: " ++ show p

someProof res =
  case res of
   Just _ -> True
   Nothing -> False
