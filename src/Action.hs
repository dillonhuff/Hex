module Action(Action,
              action,
              acApplies) where

import Data.List as L
import Data.Maybe

import Core
import Proof

data Action
  = Action {
    acApplies :: Conjecture -> Maybe ([Conjecture], [Proof] -> Proof)
    }

action = Action

applyIf :: (Conjecture -> Bool) -> Action -> Action
applyIf test a =
  action (\c -> if test c then (acApplies a) c else Nothing)

applyFirst :: [Action] -> Action
applyFirst actions =
  action $ (\c -> apFirst c $ L.map acApplies actions)

apFirst :: a -> [a -> Maybe b] -> Maybe b
apFirst c [] = Nothing
apFirst c (a:as) =
  case a c of
   Just b -> Just b
   Nothing -> apFirst c as

