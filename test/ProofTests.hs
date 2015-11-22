module ProofTests(allProofTests) where

import Test.HUnit

import Boolean
import Core
import Proof
import Nat
import Search
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
            testThm nateqNNTrue True,
            testThm bEqTrue True,
            testThm nateqZZTrue True,
            testThm nateqSZSZTrue True]

trueIsTrue = conjecture [boolDT] [] [] (trueTerm, trueTerm)
trueFuncIsTrue = conjecture [boolDT] [trueFunc] [] (trueFuncall, trueTerm)
falseIsTrue = conjecture [boolDT] [] [] (falseTerm, trueTerm)
falseFuncIsTrue = conjecture [boolDT] [falseFunc] [] (falseFuncall, trueTerm)
beqFalseFalse = conjecture [boolDT] [beq] [] (beqFuncallFF, trueTerm)
beqTrueFalse = conjecture [boolDT] [beq] [] (beqFuncallTF, trueTerm)
beqBBTrue = conjecture [boolDT] [beq] [] (beqBB, trueTerm)
beqBCFalse = conjecture [boolDT] [beq] [] (beqBC, trueTerm)
nateqNNTrue = conjecture [natDT] [nateq] [] (nateqNN, trueTerm)
bEqTrue = conjecture [boolDT] [] [(bTerm, trueTerm)] (bTerm, trueTerm)
nateqSZSZTrue = conjecture [natDT] [nateq] [] (nateqSZSZ, trueTerm)
nateqZZTrue = conjecture [natDT] [nateq] [] (nateqZZ, trueTerm)

trueFunc = function (dId "trueFunc") [] [] boolType trueTerm
falseFunc = function (dId "falseFunc") [] [] boolType falseTerm

trueFuncall = ap (dGbl "trueFunc" (func [] boolType)) []
falseFuncall = ap (dGbl "falseFunc" (func [] boolType)) []
beqFuncallFF = ap (dGbl "beq" (func [boolType, boolType] boolType)) [falseTerm, falseTerm]
beqFuncallTF = ap (dGbl "beq" (func [boolType, boolType] boolType)) [trueTerm, falseTerm]
beqBB = ap (dGbl "beq" (func [boolType, boolType] boolType)) [lcl $ dLcl "b" boolType, lcl $ dLcl "b" boolType]
beqBC = ap (dGbl "beq" (func [boolType, boolType] boolType)) [lcl $ dLcl "b" boolType, lcl $ dLcl "c" boolType]
nateqNN = ap (dGbl "nateq" (func [natType, natType] bt)) [lcl $ dLcl "n" natType, lcl $ dLcl "n" natType]
nateqSZSZ = ap (dGbl "nateq" (func [natType, natType] bt)) [sz, sz]
nateqZZ = ap (dGbl "nateq" (func [natType, natType] bt)) [ap zeroGlobal [], ap zeroGlobal []]

sz = ap sGlobal [ap zeroGlobal []]

bTerm = lcl $ dLcl "b" boolType

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
