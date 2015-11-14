module Proof(proveFile) where

import Tip.Types

import Utils

proveFile :: String -> IO (Maybe Proof)
proveFile filePath = do
  f <- readTipFile filePath
  case f of
   Left str -> return Nothing
   Right th -> return $ tryToProve $ buildConjecture th

buildConjecture th =
  let dts = thy_datatypes th in
   case dts of
    [] -> Conjecture

tryToProve :: Conjecture -> Maybe Proof
tryToProve c = error "tryToProve"

data Conjecture = Conjecture

data Proof = Proof
