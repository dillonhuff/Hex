module ProofTests(allProofTests) where

import Test.HUnit

import Boolean
import Core
import Proof
import Nat
import TestUtils
import Utils

allProofTests =
  TestList [testThm trueIsTrue True,
            testThm trueFuncIsTrue True,
            testThm falseIsTrue False,
            testThm falseFuncIsTrue False,
            testThm beqFalseFalse True,
            testThm beqTrueFalse False,
            testThm beqBBTrue True,
            testThm beqBCFalse False,
            testThm nateqNNTrue True]

trueIsTrue = conjecture [boolDT] [] trueTerm
trueFuncIsTrue = conjecture [boolDT] [trueFunc] trueFuncall
falseIsTrue = conjecture [boolDT] [] falseTerm
falseFuncIsTrue = conjecture [boolDT] [falseFunc] falseFuncall
beqFalseFalse = conjecture [boolDT] [beq] beqFuncallFF
beqTrueFalse = conjecture [boolDT] [beq] beqFuncallTF
beqBBTrue = conjecture [boolDT] [beq] beqBB
beqBCFalse = conjecture [boolDT] [beq] beqBC
nateqNNTrue = conjecture [natDT] [nateq] nateqNN

trueFunc = function (dId "trueFunc") [] [] boolType trueTerm
falseFunc = function (dId "falseFunc") [] [] boolType falseTerm

trueFuncall = ap (dGbl "trueFunc" (func [] boolType) []) []
falseFuncall = ap (dGbl "falseFunc" (func [] boolType) []) []
beqFuncallFF = ap (dGbl "beq" (func [boolType, boolType] boolType) [boolType, boolType]) [falseTerm, falseTerm]
beqFuncallTF = ap (dGbl "beq" (func [boolType, boolType] boolType) [boolType, boolType]) [trueTerm, falseTerm]
beqBB = ap (dGbl "beq" (func [boolType, boolType] boolType) [boolType, boolType]) [lcl $ dLcl "b" boolType, lcl $ dLcl "b" boolType]
beqBC = ap (dGbl "beq" (func [boolType, boolType] boolType) [boolType, boolType]) [lcl $ dLcl "b" boolType, lcl $ dLcl "c" boolType]
nateqNN = ap (dGbl "nateq" (func [natType, natType] natType) [natType, natType]) [lcl $ dLcl "n" natType, lcl $ dLcl "n" natType]

testThm thm expected = TestCase $ tf thm expected

tf thm expected = do
  let p = someProof $ tryToProve thm in
   assertBool (caseFailMsg thm p expected) (p == expected)

caseFailMsg thm p expected =
  "Input:\n" ++ pretty 0 thm ++ "\nResult: " ++ show p

someProof res =
  case res of
   Just _ -> True
   Nothing -> False

caseFile name = testPath ++ name ++ ".smt2"

testPath = "/Users/dillon/Haskell/ThmProving/Hex/test/cases/"
