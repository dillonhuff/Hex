module Action(Action,
              action,
              acApplies,
              repeatAction, applyIf, applyFirst) where

import Data.List as L
import Data.Maybe

import Core
import Proof
import Utils

data Action
  = Action {
    acApplies :: Conjecture -> Maybe ([Conjecture], [Proof] -> Proof)
    }

action = Action

applyIf :: (Conjecture -> Bool) -> Action -> Action
applyIf test a =
  action (\c -> if test c then (acApplies a) c else Nothing)

repeatAction :: Int -> Action -> Action
repeatAction n a =
  action $ (\c -> repeatAc n (acApplies a) c)

repeatAc 0 a c = error "repeatAc"
repeatAc 1 a c =
  case a c of
   Nothing -> error "repeatAc a gave nothing"
   Just ([], pf) -> error $ "Solved subgoal " ++ (pretty 0 $ pf [])
repeatAc n a c =
  case a c of
   Just ([], pf) -> Just ([], pf)
   Just ([sg], pf) -> do
     ([sg2], pf2) <- repeatAc (n-1) a sg
     return ([sg2], pf2)
   Nothing -> error "repeatAc nothing"

applyFirst :: [Action] -> Action
applyFirst actions =
  action $ (\c -> apFirst c $ L.map acApplies actions)

apFirst :: a -> [a -> Maybe b] -> Maybe b
apFirst c [] = Nothing
apFirst c (a:as) =
  case a c of
   Just b -> Just b
   Nothing -> apFirst c as

