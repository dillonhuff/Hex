module Boolean(boolDT, trueTerm) where

import Proof

boolDT = datatype (dId "Bool") [] [dataCon trueGlobal [], dataCon falseGlobal []]

boolType = tyCon (dId "Bool") []

trueTerm = ap trueGlobal []

trueGlobal = dGbl "T" boolType []
falseGlobal = dGbl "F" boolType []
