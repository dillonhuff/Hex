module Lift(liftTerm) where

import Data.List as L
import Data.Maybe

import Core
import Utils

liftTerm :: Term -> Term -> Maybe Term
liftTerm target toLift =
  case liftTerm' target toLift of
   Just (Just g, t) ->
     if existsTerm (\e -> e == target) (ap g [t]) then Just $ ap g [t] else Nothing
   Just (Nothing, t) ->
     if existsTerm (\e -> e == target) t then Just $ t else Nothing
   Nothing -> Nothing

liftTerm' :: Term -> Term -> Maybe (Maybe Global, Term)
liftTerm' target toLift =
  case target == toLift of
   True -> Just (Nothing, target)
   False ->
     case isAp target && isAp toLift of
      True ->
        case (callHead target) == (callHead toLift) of
         True -> do
           res <- mergeArgs target toLift
           return (Nothing, res)
         False -> Nothing
      False ->
        case isAp toLift &&
             L.length (callArgs toLift) == 1 &&
             (L.head $ callArgs toLift) == target of
         True -> Just (Just $ callHead toLift, L.head $ callArgs toLift)
         False -> Nothing

mergeArgs t l =
  let argRes = L.zipWith liftTerm' (callArgs t) (callArgs l)
      mergedArgs = catMaybes argRes
      liftedCalls = catMaybes $ L.map fst mergedArgs
      oldHead = callHead t in
   case L.length mergedArgs == L.length (callArgs t) of
    True ->
      case L.length liftedCalls of
       1 ->
         let resType = returnType $ gblType $ oldHead
             liftedType = func [resType] resType
             lifted = L.head liftedCalls in
          case liftedType == gblType lifted of
           True -> Just $ ap lifted [ap oldHead $ L.map snd mergedArgs]
           False -> Nothing
       0 -> Just $ ap oldHead $ L.map snd mergedArgs
       _ -> Nothing
    False -> Nothing
