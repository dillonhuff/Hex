module PAScratch where

import Data.Maybe

import Action
import ProofAnalysis
import Nat
import Utils
import TacticProve

pp t = putStrLn $ pretty 0 $ t
pProof thm = putStrLn $ pretty 0 $ fromJust $ tacticProve thm
pThm thm = putStrLn $ pretty 0 $ thm
apAc a thm = putStrLn $ pretty 0 $ fst $ fromJust $ (acApplies a) thm
sgs a thm = fst $ fromJust $ (acApplies a) thm
