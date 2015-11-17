module Boolean(boolDT,
               boolType, bt,
               beq,
               trueTerm,
               falseTerm) where

import Core
import Proof
import Utils

boolDT = datatype (dId "Bool") [] [dataCon trueGlobal [], dataCon falseGlobal []]

boolType = tyCon (dId "Bool") []
bt = boolType

trueTerm = ap trueGlobal []
falseTerm = ap falseGlobal []

trueGlobal = dGbl "T" bt []
falseGlobal = dGbl "F" bt []

beq = function (dId "beq") [] beqArgs bt beqBody

beqArgs = [dLcl "b1" bt, dLcl "b2" bt]

beqBody = match (lcl $ dLcl "b1" bt) [trueBeq, falseBeq]

trueBeq =
  alt tPat $ match (lcl $ dLcl "b2" bt) [alt tPat trueTerm, alt fPat falseTerm]
falseBeq =
  alt fPat $ match (lcl $ dLcl "b2" bt) [alt tPat falseTerm, alt fPat trueTerm]

tPat = conPat trueGlobal []
fPat = conPat falseGlobal []
