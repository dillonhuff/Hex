module Utils(Pretty(..),
             ps, spaces, indent,
             prettyEq) where

import Data.List as L

class Pretty a where
  pretty :: Int -> a -> String

instance Pretty a => Pretty [a] where
  pretty n as = L.concat $ L.intersperse "\n" $ L.map (pretty n) as

instance Pretty a => Pretty (Maybe a) where
  pretty n Nothing = "Nothing"
  pretty n (Just a) = pretty n a

instance (Pretty a, Pretty b) => Pretty (a, b) where
  pretty n (x, y) = "(\n" ++ pretty n x ++ "\n" ++ pretty n y

prettyEq :: Pretty a => Int -> (a, a) -> String
prettyEq n (l, r) = (indent n "(=") ++ "\n" ++
                    (indent n $ pretty (n+1) l) ++ "\n" ++
                    (indent n $ pretty (n+1) r) ++ ")\n"
--                    ps $ spaces ["=", pretty (n+1) l, pretty (n+1) r]

ps str = "(" ++ str ++ ")"
spaces strs = L.concat $ L.intersperse " " strs
indent n str = "\n" ++ (L.concat $ L.replicate n "  ") ++ str
