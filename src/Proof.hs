module Proof(conjecture,
             tryToProve,
             ap,
             dataCon,
             datatype,
             tyCon,
             dId,
             dGbl) where

import Data.List as L

tryToProve :: Conjecture -> Maybe Proof
tryToProve c = tryToProve' eqTerm c

tryToProve' :: (Conjecture -> Maybe Proof) -> Conjecture -> Maybe Proof
tryToProve' f c = f c

eqTerm t =
  case conjAssert t of
   gbl Proof.:@: [] -> if sameName gbl trueGbl then Just Proof else Nothing
   _ -> Nothing

sameName (Global id1 _ _) (Global id2 _ _) = (idName id1) == (idName id2)

trueGbl = Global trueId boolType []

trueId = dId "T"

boolPolyType = error "boolPolyType"

boolType = error "boolType" []

data Conjecture =
  Conjecture {
    conjDataTypes :: [Datatype],
    conjFunctions :: [Function],
    conjAssert :: Term
    } deriving (Eq, Ord, Show)

conjecture = Conjecture

data Proof = Proof
  
data Function =
  Function {
    funcName :: Id,
    funcTVS :: [Id],
    funcArgs :: [Local],
    funcRes :: Type,
    funcBody :: Term
    } deriving (Eq, Ord, Show)

data Datatype =
  Datatype {
    dtName :: Id,
    dtTVS :: [Id],
    dtConstructors :: [DataCon]
    } deriving (Eq, Ord, Show)

datatype = Datatype

data DataCon =
  DataCon {
    dcName :: Global,
    dcArgs :: [Type]
    } deriving (Eq, Ord, Show)

dataCon = DataCon

data Term =
  Lcl Local |
  Global :@: [Term] |
  Match Term [Alt]
  deriving (Eq, Ord, Show)

ap g args = g :@: args

data Alt = Alt { casePat :: Pat, caseRHS :: Term }
             deriving (Eq, Ord, Show)
  
data Pat = Default | ConPat { patCon :: Global, patArgs :: [Local] }
         deriving (Eq, Ord, Show)

data Type = TyVar Id | TyCon Id [Type]
          deriving (Eq, Ord, Show)

tyCon = TyCon

data Global =
  Global { gblName :: Id, gblType :: Type, gblArgs :: [Type] }
  deriving (Eq, Ord, Show)

global = Global

dGbl n tp args = global (dId n) tp args

data Local =
  Local { lclName :: Id, lclType :: Type }
  deriving (Eq, Ord, Show)

data Id =
  Id { idName :: String, idFile :: String, idPos :: (Int, Int) }
  deriving (Eq, Ord, Show)

dId n = Id n "NO_FILE" (-1, -1)
