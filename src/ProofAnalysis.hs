module ProofAnalysis(commonEndGames,
                     endGameCounts) where

import Data.List as L

import Proof

endGameCounts psl =
  L.map (\ps -> (depth $ L.head ps, L.length ps)) $ commonEndGames psl

commonEndGames :: [Proof] -> [[Proof]]
commonEndGames ps =
  let subProofs = L.concatMap subproofs ps in
   L.groupBy (\p1 p2 -> depth p1 == depth p2) $ L.sortBy (\p1 p2 -> compare (depth p1) (depth p2)) subProofs
