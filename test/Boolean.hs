module Boolean(boolDT,
               boolType, bt,
               beq,
               trueTerm,
               falseTerm,
               boolThms, boolNonThms,
               trueIsTrue,
               bEqTrue,
               trueFuncIsTrue,
               beqFalseFalse,
               beqBBTrue) where
import Core
import Proof
import Utils

boolDT = datatype (dId "Bool") [] [dataCon trueGlobal [], dataCon falseGlobal []]

boolType = tyCon (dId "Bool") []
bt = boolType

trueTerm = ap trueGlobal []
falseTerm = ap falseGlobal []

trueGlobal = dGbl "T" (func [] bt)
falseGlobal = dGbl "F" (func [] bt)

beq = function (dId "beq") [] beqArgs bt beqBody

beqArgs = [dLcl "b1" bt, dLcl "b2" bt]

beqBody = match (lcl $ dLcl "b1" bt) [trueBeq, falseBeq]

trueBeq =
  alt tPat $ match (lcl $ dLcl "b2" bt) [alt tPat trueTerm, alt fPat falseTerm]
falseBeq =
  alt fPat $ match (lcl $ dLcl "b2" bt) [alt tPat falseTerm, alt fPat trueTerm]

tPat = conPat trueGlobal []
fPat = conPat falseGlobal []

-- Test theorems about booleans
boolThms = [trueIsTrue,
            bEqTrue,
            trueFuncIsTrue,
            beqFalseFalse,
            beqBBTrue]

boolNonThms = [falseIsTrue,
               falseFuncIsTrue,
               beqTrueFalse,
               beqBCFalse]

trueIsTrue = conjecture [boolDT] [] [] (trueTerm, trueTerm)
trueFuncIsTrue = conjecture [boolDT] [trueFunc] [] (trueFuncall, trueTerm)
falseIsTrue = conjecture [boolDT] [] [] (falseTerm, trueTerm)
falseFuncIsTrue = conjecture [boolDT] [falseFunc] [] (falseFuncall, trueTerm)
beqFalseFalse = conjecture [boolDT] [beq] [] (beqFuncallFF, trueTerm)
beqTrueFalse = conjecture [boolDT] [beq] [] (beqFuncallTF, trueTerm)
beqBBTrue = conjecture [boolDT] [beq] [] (beqBB, trueTerm)
beqBCFalse = conjecture [boolDT] [beq] [] (beqBC, trueTerm)
bEqTrue = conjecture [boolDT] [] [(bTerm, trueTerm)] (bTerm, trueTerm)
trueFunc = function (dId "trueFunc") [] [] boolType trueTerm
falseFunc = function (dId "falseFunc") [] [] boolType falseTerm

trueFuncall = ap (dGbl "trueFunc" (func [] boolType)) []
falseFuncall = ap (dGbl "falseFunc" (func [] boolType)) []
beqFuncallFF = ap (dGbl "beq" (func [boolType, boolType] boolType)) [falseTerm, falseTerm]
beqFuncallTF = ap (dGbl "beq" (func [boolType, boolType] boolType)) [trueTerm, falseTerm]
beqBB = ap (dGbl "beq" (func [boolType, boolType] boolType)) [lcl $ dLcl "b" boolType, lcl $ dLcl "b" boolType]
beqBC = ap (dGbl "beq" (func [boolType, boolType] boolType)) [lcl $ dLcl "b" boolType, lcl $ dLcl "c" boolType]

bTerm = lcl $ dLcl "b" boolType
