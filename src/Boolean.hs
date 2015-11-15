module Boolean(boolDT,
               boolType,
               trueTerm,
               falseTerm) where

import Core
import Proof

boolDT = datatype (dId "Bool") [] [dataCon trueGlobal [], dataCon falseGlobal []]

boolType = tyCon (dId "Bool") []

trueTerm = ap trueGlobal []
falseTerm = ap falseGlobal []

trueGlobal = dGbl "T" boolType []
falseGlobal = dGbl "F" boolType []

