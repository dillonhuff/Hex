module Utils(Pretty(..), ps, spaces) where

import Data.List as L

class Pretty a where
  pretty :: Int -> a -> String

instance Pretty a => Pretty [a] where
  pretty n as = L.concat $ L.intersperse "\n" $ L.map (pretty n) as
  
ps str = "(" ++ str ++ ")"
spaces strs = L.concat $ L.intersperse " " strs
