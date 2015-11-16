module Core(Conjecture,
            conjecture,
            eqTerm,
            conjDataTypes, conjFunctions, conjAssert,
            function,
            funcBody, funcName, funcArgs,
            genSub, sameFunc, replaceFunc,
            ap,
            dataCon,
            datatype,
            tyCon, func,
            dId,
            idName,
            dGbl,
            global,
            sameName) where

import Data.List as L

import Utils

data Conjecture =
  Conjecture {
    conjDataTypes :: [Datatype],
    conjFunctions :: [Function],
    conjAssert :: Term
    } deriving (Eq, Ord, Show)

instance Pretty Conjecture where
  pretty n c = (pretty n (conjDataTypes c)) ++ "\n" ++
               (pretty n (conjFunctions c)) ++ "\n" ++
               (pretty n (conjAssert c))

conjecture = Conjecture

eqTerm c =
  case conjAssert c of
   gbl :@: [] -> sameName gbl trueGbl
   _ -> False

trueGbl = global trueId boolType []
trueId = dId "T"
boolType = tyCon (dId "Bool") []

data Function =
  Function {
    funcName :: Id,
    funcTVS :: [Id],
    funcArgs :: [Local],
    funcRes :: Type,
    funcBody :: Term
    } deriving (Eq, Ord, Show)

function = Function

instance Pretty Function where
  pretty n f = ps $ spaces $ ["define-fun", pretty 0 (funcName f), ps ""]
  
data Datatype =
  Datatype {
    dtName :: Id,
    dtTVS :: [Id],
    dtConstructors :: [DataCon]
    } deriving (Eq, Ord, Show)

instance Pretty Datatype where
  pretty n d = ps $ spaces $ ["declare-datatypes", ps "", ps $ ps $ pretty 0 $ dtName d]
  
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

instance Pretty Term where
  pretty n t = ""
  
ap g args = g :@: args

sameFunc f (g :@: _) = f == (gblName g)

replaceFunc t (_ :@: _) = t

genSub :: (Term -> Bool) -> (Term -> Term) -> Term -> Term
genSub test replace t = if test t then replace t else t

data Alt = Alt { casePat :: Pat, caseRHS :: Term }
             deriving (Eq, Ord, Show)
  
data Pat = Default | ConPat { patCon :: Global, patArgs :: [Local] }
         deriving (Eq, Ord, Show)

data Type = TyVar Id | TyCon Id [Type] | Func [Type] Type
          deriving (Eq, Ord, Show)

tyCon = TyCon
func = Func

data Global =
  Global { gblName :: Id, gblType :: Type, gblArgs :: [Type] }
  deriving (Eq, Ord, Show)

global = Global

dGbl n tp args = global (dId n) tp args

sameName (Global id1 _ _) (Global id2 _ _) = (idName id1) == (idName id2)

data Local =
  Local { lclName :: Id, lclType :: Type }
  deriving (Eq, Ord, Show)

data Id =
  Id { idName :: String, idFile :: String, idPos :: (Int, Int) }
  deriving (Eq, Ord, Show)

dId n = Id n "NO_FILE" (-1, -1)

instance Pretty Id where
  pretty n id = idName id
