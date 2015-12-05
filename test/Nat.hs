module Nat(natDT,
           nv, nt, sGlobal, zeroGlobal,
           natplusGbl,
           natType,
           nateq, natplus, s, z, natp,
           natThms,
           natplusComm, natplusCommBase, natplusZbcTrue,
           nateqSZSZTrue,
           sasbTrue, natplusCommInd, natplusRewrite,
           natplusSLeft, natplusSRight, npZSBEqSnpAB) where

import Boolean
import Core

natDT = datatype (dId "Nat") [] [dataCon zeroGlobal [], dataCon sGlobal [natType]]

zeroGlobal = dGbl "Z" (func [] nt)
sGlobal = dGbl "S" (func [nt] nt)
nateq = function (dId "nateq") [] nateqArgs bt nateqBody
natplus = function (dId "natplus") [] natplusArgs nt natplusBody

nateqArgs = [dLcl "n1" nt, dLcl "n2" nt]
natplusArgs = [dLcl "n1" nt, dLcl "n2" nt]

nv str  = lcl $ dLcl str nt

n1 = lcl $ dLcl "n1" nt
n2 = lcl $ dLcl "n2" nt

nateqBody = match n1 [alt zPat zTerm, alt (sPat "p1")  sTerm]
natplusBody =
  match n1 [alt zPat n2, alt (sPat "p1")  (ap sGlobal [ap natplusGbl [nv "p1", n2]])]

natplusGbl = (dGbl "natplus" (func [nt, nt] nt))

zPat = conPat zeroGlobal []
sPat n = conPat sGlobal [dLcl n nt]

zTerm = match n2 [alt zPat trueTerm, alt (sPat "p2") falseTerm]
sTerm = match n2 [alt zPat falseTerm, alt (sPat "p2") (ap (dGbl "nateq" (func [nt, nt] bt)) [nv "p1", nv "p2"])]

natType = tyCon (dId "Nat") []

nt = natType

-- Test theorems about natural numbers
natThms = [nateqNNTrue,
           nateqZZTrue,
           nateqSZSZTrue,
           natplusZNTrue,
           natplusNZTrue,
           sasbTrue,
           natplusComm,
           natplusCommBase,
           natplusCommInd,
           natplusZbcTrue,
           natplusRewrite,
           natplusSLeft,
           natplusSRight,
           npZSBEqSnpAB,
           natplusAssoc]

nateqNNTrue = conjecture [natDT] [nateq] [] (nateqNN, trueTerm)
nateqSZSZTrue = conjecture [natDT] [nateq] [] (nateqSZSZ, trueTerm)
nateqZZTrue = conjecture [natDT] [nateq] [] (nateqZZ, trueTerm)
natplusZNTrue = conjecture [natDT] [natplus] [] (natplusZN, nv "n")
natplusNZTrue = conjecture [natDT] [natplus] [] (natplusNZ, nv "n")
sasbTrue = conjecture [natDT] [] [(nv "a", nv "b")] (s $ nv "a", s $ nv "b")
natplusComm = conjecture [natDT] [natplus] [] (ap natplusGbl [nv "a", nv "b"], ap natplusGbl [nv "b", nv "a"])
natplusCommBase = conjecture [natDT] [natplus] [] (natp z (nv "b"), natp (nv "b") z)
natplusCommInd = conjecture [natDT] [natplus] [(natp (nv "$0") (nv "b"), natp (nv "b") (nv "$0"))] (natp (s (nv "$0")) (nv "b"), natp (nv "b") (s (nv "$0")))
natplusZbcTrue = conjecture [natDT] [natplus] [(nv "b", nv "c")] (natp z (nv "b"), nv "c")
natplusRewrite =
  conjecture [natDT] [natplus] assumptions (natp (s $ nv "a") (nv "b"), natp (nv "b") (s $ nv "a"))
  where
    assumptions = [(natp (nv "a") (nv "b"), natp (nv "b") (nv "a")),
                   (natp (s $ nv "a") (nv "b"), s $ natp (nv "a") (nv "b")),
                   (natp (nv "b") (s $ nv "a"), s $ natp (nv "b") (nv "a"))]
natplusSLeft = conjecture [natDT] [natplus] [] (natp (s $ nv "a") (nv "b"), s $ natp (nv "a") (nv "b"))
natplusSRight = conjecture [natDT] [natplus] [] (natp (nv "a") (s $ nv "b"), s $ natp (nv "a") (nv "b"))
npZSBEqSnpAB = conjecture [natDT] [natplus] [] (natp z (s $ nv "b"), s $ natp z (nv "b"))
natplusAssoc = conjecture [natDT] [natplus] [] (natp (nv "a") (natp (nv "b") (nv "c")), natp (natp (nv "a") (nv "b")) (nv "c"))

nateqNN = ap (dGbl "nateq" (func [natType, natType] bt)) [lcl $ dLcl "n" natType, lcl $ dLcl "n" natType]
nateqSZSZ = ap (dGbl "nateq" (func [natType, natType] bt)) [sz, sz]
nateqZZ = ap (dGbl "nateq" (func [natType, natType] bt)) [ap zeroGlobal [], ap zeroGlobal []]
natplusZN = ap natplusGbl [ap zeroGlobal [], nv "n"]
natplusNZ = ap natplusGbl [nv "n", ap zeroGlobal []]

natp l r = ap natplusGbl [l, r]
sz = ap sGlobal [ap zeroGlobal []]
s v = ap sGlobal [v]
z = ap zeroGlobal []
