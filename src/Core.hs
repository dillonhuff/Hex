module Core(Conjecture,
            conjecture,
            eqTerm, swapConj,
            conjDataTypes, conjFunctions, conjAssumptions, conjAssert,
            function, trueTermC,
            funcBody, funcName, funcArgs,
            genSub, sameFunc, replaceFunc,
            Term,
            lcl, ap, match,
            callHead, callArgs,
            existsTerm, isDataConMatch, isDataCon, selectMatch,
            lclType, lclName, isLcl, isFuncall, isConstructorCall,
            isMatch,
            getLocal, collectFromTerms, matchedTerm,
            isAp, termType,
            noFreeVars, freeVars,
            alt,
            conPat,
            dataCon,
            dcName, dcArgs,
            datatype,
            dtName, dtConstructors,
            tyCon, func,
            typesMatch,
            isFuncType, returnType, argType, argTypes, arity,
            tyConName,
            dId,
            idName,
            local, dLcl,
            Global,
            global, dGbl, gblName, gblType,
            sameName) where

import Data.List as L

import Utils

data Conjecture =
  Conjecture {
    conjDataTypes :: [Datatype],
    conjFunctions :: [Function],
    conjAssumptions :: [(Term, Term)],
    conjAssert :: (Term, Term)
    } deriving (Eq, Ord, Show)

instance Pretty Conjecture where
  pretty n c = (indent n $ pretty n (conjDataTypes c)) ++ "\n" ++
               (indent n $ pretty n (conjFunctions c)) ++ "\n" ++
               (L.concatMap (\(t1, t2) -> indent n $ ps $ spaces ["=", pretty n t1, pretty n t2]) $ conjAssumptions c) ++
               (indent n $ ps $ spaces ["=", pretty n (fst $ conjAssert c), pretty n (snd $ conjAssert c)])

conjecture = Conjecture

eqTerm c =
  case fst $ conjAssert c of
   gbl :@: [] -> sameName gbl trueGbl
   _ -> False

swapConj c = c { conjAssert = swap $ conjAssert c }

swap (l, r) = (r, l)

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
  pretty n f = ps $ (spaces ["define-fun", pretty 0 (funcName f), prettyArgs $ funcArgs f, pretty 0 $ funcRes f]) ++
               (pretty (n+1) $ funcBody f)

prettyArgs args = ps $ spaces $ L.map prettyArg args

prettyArg a = ps $ spaces [pretty 0 a, pretty 0 $ lclType a]

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

termType (Lcl l) = lclType l
termType (g :@: _) = returnType $ gblType g
termType (Match t alts) = termType $ caseRHS $ L.head alts

lcl = Lcl

ap g as =
  case typesMatch (argTypes $ gblType g) (L.map termType as) of
   True -> g :@: as
   False -> error $ "ap with bad types\n" ++ pretty 0 g ++ "\n" ++ pretty 0 as

typesMatch [] [] = True
typesMatch a [] = False
typesMatch [] b = False
typesMatch [a] [b] = a == b
typesMatch (a:as) (b:bs) = a == b && typesMatch as bs

match = Match

isLcl (Lcl _) = True
isLcl _ = False

isAp (g :@: _) = True
isAp _ = False

isMatch (Match _ _) = True
isMatch _ = False

isFuncall c (g :@: _) = L.elem (gblName g) $ L.map funcName $ conjFunctions c
isFuncall _ _ = False

isConstructorCall c t = not $ isFuncall c t

getLocal (Lcl l) = l

callHead (g :@: _) = g
callArgs (_ :@: ts) = ts

matchedTerm (Match t _) = t

collectFromTerms :: (Term -> [a]) -> Term -> [a]
collectFromTerms f t@(g :@: ts) = (f t) ++ L.concatMap (collectFromTerms f) ts
collectFromTerms f t@(Match e alts) =
  (f t) ++ (f e) ++ (L.concatMap (collectFromTerms f) $ L.map caseRHS alts)
collectFromTerms f t@(Lcl l) = f t

noFreeVars t = (freeVars t) == []

freeVars t = freeVars' [] t

freeVars' :: [Term] -> Term -> [Term]
freeVars' vs (g :@: ts) = L.concatMap (freeVars' vs) ts
freeVars' vs (Match e alts) = (freeVars' vs e) ++ (L.concatMap (altFreeVars vs) alts)
freeVars' vs (Lcl l) = if not $ L.elem (Lcl l) vs then [Lcl l] else []

altFreeVars vs alt =
  let nvs = L.map Lcl $ patArgs $ casePat alt in
   freeVars' (nvs ++ vs) $ caseRHS alt

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

instance Pretty Type where
  pretty n (TyCon i []) = pretty 0 i

tyCon = TyCon
func = Func

tyConName (TyCon n _) = n

returnType (Func _ t) = t

isFuncType (Func _ _) = True
isFuncType _ = False

arity (Func args _) = L.length args

argType n (Func args _) = args !! n

argTypes (Func args _) = args

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
