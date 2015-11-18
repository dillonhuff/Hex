module Core(Conjecture,
            conjecture,
            eqTerm,
            conjDataTypes, conjFunctions, conjAssumptions, conjAssert,
            function, trueTermC,
            funcBody, funcName, funcArgs,
            genSub, sameFunc, replaceFunc,
            lcl, ap, match,
            callHead, callArgs,
            existsTerm, isDataConMatch, selectMatch,
            lclType, lclName, isLcl, isFuncall, getLocal, collectFromTerms,
            alt,
            conPat,
            dataCon,
            dcName, dcArgs,
            datatype,
            dtName, dtConstructors,
            tyCon, func,
            tyConName,
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
    conjAssumptions :: [(Term, Term)],
    conjAssert :: Term
    } deriving (Eq, Ord, Show)

instance Pretty Conjecture where
  pretty n c = (indent n $ pretty n (conjDataTypes c)) ++ "\n" ++
               (indent n $ pretty n (conjFunctions c)) ++ "\n" ++
               (L.concatMap (\(t1, t2) -> indent n $ pretty n t1 ++ " = " ++ pretty n t2) $ conjAssumptions c) ++
               (indent n $ pretty n (conjAssert c))

conjecture = Conjecture

eqTerm c =
  case conjAssert c of
   gbl :@: [] -> sameName gbl trueGbl
   _ -> False

trueTermC = ap trueGbl []
trueGbl = global trueId (func [] boolType)
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

isLcl (Lcl _) = True
isLcl _ = False

isFuncall c (g :@: _) = L.elem (gblName g) $ L.map funcName $ conjFunctions c
isFuncall _ _ = False

getLocal (Lcl l) = l

callHead (g :@: _) = g
callArgs (_ :@: ts) = ts

collectFromTerms :: (Term -> [a]) -> Term -> [a]
collectFromTerms f t@(g :@: ts) = (f t) ++ L.concatMap (collectFromTerms f) ts
collectFromTerms f t@(Match e alts) =
  (f t) ++ (f e) ++ (L.concatMap (collectFromTerms f) $ L.map caseRHS alts)
collectFromTerms f t@(Lcl l) = f t

existsTerm :: (Term -> Bool) -> Term -> Bool
existsTerm f t@(g :@: ts) =
  if f t then True else L.or $ L.map (existsTerm f) ts
existsTerm f t@(Match e alts) =
  let altsEx = L.or $ (f e):(L.map (\a -> existsTerm f $ caseRHS a) alts) in
   f t || altsEx
existsTerm f t = f t

isDataConMatch c (Match t _) = isDataCon c t
isDataConMatch _ _ = False

isDataCon c (g :@: _) =
  let dataCons = L.concatMap dtConstructors $ conjDataTypes c in
   L.elem g $ L.map dcName dataCons
isDataCon _ _ = False

selectMatch (Match t alts) =
  let dc = callHead t
      args = callArgs t
      alt = L.head $ L.filter (\a -> (patCon $ casePat a) == dc) alts
      pat = casePat alt
      fps = patArgs pat
      rhs = caseRHS alt
      subPairs = L.zip fps args in
   L.foldr replaceParam rhs $ subPairs
  
sameFunc f (g :@: _) = f == (gblName g)
sameFunc _ _ = False

replaceFunc body formalParams (_ :@: args) =
  L.foldr replaceParam body $ L.zip formalParams args

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

data Pat = ConPat { patCon :: Global, patArgs :: [Local] }
         deriving (Eq, Ord, Show)

instance Pretty Pat where
  pretty n (ConPat g ls) = ps $ spaces $ [pretty n g] ++ (L.map (pretty n) ls)

conPat = ConPat

data Type = TyVar Id | TyCon Id [Type] | Func [Type] Type
          deriving (Eq, Ord, Show)

tyCon = TyCon
func = Func

tyConName (TyCon n _) = n

data Global =
  Global { gblName :: Id, gblType :: Type }
  deriving (Eq, Ord, Show)

instance Pretty Global where
  pretty n (Global name _) = pretty n name

global = Global

dGbl n tp = global (dId n) tp

sameName (Global id1 _) (Global id2 _) = (idName id1) == (idName id2)

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
