module Proof(proveFile) where

import Data.List as L
import Tip.Parser
import Tip.Types

import Utils

proveFile :: String -> IO (Maybe Proof)
proveFile filePath = do
  f <- readTipFile filePath
  case f of
   Left str -> error $ "Parse error " ++ show str
   Right th -> return $ tryToProve $ buildConjecture th

buildConjecture th =
  let datatypes = L.map convertDatatype $ thy_datatypes th
      functions = L.map convertFunction $ thy_funcs th
      asserts = convertFirstAssert $ thy_asserts th in
   conjecture datatypes functions asserts

tryToProve :: Conjecture -> Maybe Proof
tryToProve c = Just Proof

convertFirstAssert [a] = convertAssert a

convertDatatype dts = error "convertDataType"

convertFunction f = error "convertFunction"

convertAssert a = error "convertAssert"

data Conjecture =
  Conjecture {
    conjDataTypes :: [Datatype Id],
    conjFunctions :: [Func],
    conjAssert :: Term
    } deriving (Eq, Ord, Show)

conjecture = Conjecture

data Proof = Proof
  
data Func =
  Func {
    funcName :: Id,
    funcTVS :: [Id],
    funcArgs :: [Local Id],
    funcRes :: Type Id,
    funcBody :: Term
    } deriving (Eq, Ord, Show)

data Term =
  Lcl (Local Id) |
  (Global Id) :@: [Term] |
  Match Term [Alt]
  deriving (Eq, Ord, Show)

data Alt = Alt { casePat :: Pat, caseRHS :: Term }
             deriving (Eq, Ord, Show)
  
data Pat = Default | ConPat { patCon :: Global Id, patArgs :: [Local Id] }
         deriving (Eq, Ord, Show)
