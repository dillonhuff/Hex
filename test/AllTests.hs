module AllTests(main) where

import Test.HUnit

import SearchTests

main = do
  runTestTT allSearchTests
