module Core(Conjecture,
            conjecture,
            eqTerm,
            conjDataTypes, conjFunctions, conjAssert,
            function,
            funcBody, funcName, funcArgs,
            genSub, sameFunc, replaceFunc,
            lcl, ap, match,
            alt,
            conPat,
            dataCon,
            datatype,
            tyCon, func,
            dId,
            idName,
            local, dLcl,
            global, dGbl,
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
  pretty n f = ps $ (spaces ["define-fun", pretty 0 (funcName f), ps ""]) ++
               (pretty (n+1) $ funcBody f)
  
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
  pretty n (Lcl l) = pretty 0 l
  pretty n (g :@: terms) = ps $ pretty 0 g ++ " " ++ (spaces $ L.map (pretty 0) terms)
  pretty n (Match e alts) = indent n $ ps $ "match " ++ pretty 0 e ++ (L.concatMap (pretty (n+1)) alts)
  
lcl = Lcl
ap g args = g :@: args
match = Match

sameFunc f (g :@: _) = f == (gblName g)

replaceFunc body formalParams (_ :@: args) =
  L.foldr replaceParam body $ L.zip formalParams args
  where
    replaceParam = \(fp, e) b -> genSub (localWithName fp) (\t -> e) b

localWithName n (Lcl l) = n == l
localWithName _ _ = False

genSub test replace t@(g :@: ts) =
  if test t then genSub' test replace t else g :@: (L.map (genSub test replace) ts)
genSub test replace t@(Match e alts) =
  if test t then genSub' test replace t else match (genSub test replace e) $ L.map (\a -> a { caseRHS = genSub test replace $ caseRHS a}) alts
genSub test replace t = genSub' test replace t

genSub' :: (Term -> Bool) -> (Term -> Term) -> Term -> Term
genSub' test replace t = if test t then replace t else t

data Alt = Alt { casePat :: Pat, caseRHS :: Term }
             deriving (Eq, Ord, Show)

instance Pretty Alt where
  pretty n (Alt p t) = indent n $ ps $ pretty n p ++ " " ++ (pretty (n+1) t)

alt = Alt

data Pat = Default | ConPat { patCon :: Global, patArgs :: [Local] }
         deriving (Eq, Ord, Show)

instance Pretty Pat where
  pretty n (ConPat g ls) = ps $ spaces $ [pretty n g] ++ (L.map (pretty n) ls)

conPat = ConPat

data Type = TyVar Id | TyCon Id [Type] | Func [Type] Type
          deriving (Eq, Ord, Show)

tyCon = TyCon
func = Func

data Global =
  Global { gblName :: Id, gblType :: Type, gblArgs :: [Type] }
  deriving (Eq, Ord, Show)

instance Pretty Global where
  pretty n (Global name _ _) = pretty n name

global = Global

dGbl n tp args = global (dId n) tp args

sameName (Global id1 _ _) (Global id2 _ _) = (idName id1) == (idName id2)

data Local =
  Local { lclName :: Id, lclType :: Type }
  deriving (Eq, Ord, Show)

instance Pretty Local where
  pretty n (Local name t) = pretty n name
  
local = Local
dLcl n tp = local (dId n) tp

data Id =
  Id { idName :: String, idFile :: String, idPos :: (Int, Int) }
  deriving (Eq, Ord, Show)

dId n = Id n "NO_FILE" (-1, -1)

instance Pretty Id where
  pretty n id = idName id
