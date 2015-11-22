module Nat(natDT,
           nv, nt, sGlobal, zeroGlobal,
           natplusGbl,
           natType,
           nateq, natplus) where

import Boolean
import Core

natDT = datatype (dId "Nat") [] [dataCon zeroGlobal [], dataCon sGlobal [natType]]

zeroGlobal = dGbl "Z" (func [] nt)
sGlobal = dGbl "S" (func [nt] nt)
nateq = function (dId "nateq") [] nateqArgs bt nateqBody
natplus = function (dId "natplus") [] natplusArgs bt natplusBody

nateqArgs = [dLcl "n1" nt, dLcl "n2" nt]
natplusArgs = [dLcl "n1" nt, dLcl "n2" nt]

nv str  = lcl $ dLcl str nt

n1 = lcl $ dLcl "n1" nt
n2 = lcl $ dLcl "n2" nt

nateqBody = match n1 [alt zPat zTerm, alt (sPat "p1")  sTerm]
natplusBody =
  match n1 [alt zPat n1, alt (sPat "p1")  (ap sGlobal [ap natplusGbl [nv "p1", n2]])]

natplusGbl = (dGbl "natplus" (func [nt, nt] nt))

zPat = conPat zeroGlobal []
sPat n = conPat sGlobal [dLcl n nt]

zTerm = match n2 [alt zPat trueTerm, alt (sPat "p2") falseTerm]
sTerm = match n2 [alt zPat falseTerm, alt (sPat "p2") (ap (dGbl "nateq" (func [nt, nt] bt)) [nv "p1", nv "p2"])]

natType = tyCon (dId "Nat") []

nt = natType
