module Lift(liftTerm) where

import Data.List as L
import Data.Maybe

import Core
import Utils

liftTerm :: Term -> Term -> Maybe Term
liftTerm target toLift =
  case liftUnary target toLift of
   Just (gs, t) -> apList gs t
   Nothing -> Nothing

apList [] t = Just t
apList (g:gs) t =
  case typesMatch (argTypes $ gblType g) [termType t] of
   True -> apList gs (ap g [t])
   False -> Nothing

liftUnary :: Term -> Term -> Maybe ([Global], Term)
liftUnary t l =
  case t == l of
   True -> Just ([], t)
   False ->
     case isAp t && isAp l && (callHead t) == (callHead l) of
      True ->
        let h = callHead t
            ta = callArgs t
            la = callArgs l
            res = sequence $ L.zipWith liftUnary ta la in
         case res of
          Just a ->
            let calls = L.concatMap fst a
                args = L.map snd a in
             Just (calls, ap h args)
          Nothing -> Nothing
      False ->
        case isAp l && (length $ callArgs l) == 1 && ((callArgs l) !! 0) == t
             && (tToT $ gblType $ callHead l) of
         True -> Just ([callHead l], t)
         False -> Nothing

tToT t = isFuncType t && (arity t == 1) && (returnType t == argType 0 t)
