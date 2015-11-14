module Utils(readTipFile) where

import Tip.HaskellFrontend

path = "/Users/dillon/Haskell/ThmProving/tip/benchmarks/benchmarks/tip2015/int_add_ident_left.smt2"

readTipFile f = readHaskellOrTipFile f defaultParams
