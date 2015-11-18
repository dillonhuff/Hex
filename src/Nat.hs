module Nat(natDT,
           sGlobal, zeroGlobal,
           natType,
           nateq) where

import Boolean
import Core

natDT = datatype (dId "Nat") [] [dataCon zeroGlobal [], dataCon sGlobal [natType]]

zeroGlobal = dGbl "Z" (func [] nt)
sGlobal = dGbl "S" (func [nt] nt)
nateq = function (dId "nateq") [] nateqArgs bt nateqBody

nateqArgs = [dLcl "n1" nt, dLcl "n2" nt]

n1 = lcl $ dLcl "n1" nt
n2 = lcl $ dLcl "n2" nt
nateqBody = match n1 [alt zPat zTerm, alt (sPat "p1")  sTerm]

zPat = conPat zeroGlobal []
sPat n = conPat sGlobal [dLcl n nt]

zTerm = match n2 [alt zPat trueTerm, alt (sPat "p2") falseTerm]
sTerm = match n2 [alt zPat falseTerm, alt (sPat "p2") (ap (dGbl "nateq" (func [nt, nt] bt)) [lcl $ dLcl "p1" nt, lcl $ dLcl "p2" nt])]

natType = tyCon (dId "Nat") []

nt = natType
