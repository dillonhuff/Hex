module Proof(Proof,
             eqProof, substituteProof, unfoldProof,
             selectProof, splitVarProof, inductionProof, symmetryProof,
             modusPonensProof,
             subproofs, depth,
             Conjecture) where

import Data.List as L

import Core
import Utils

data Proof
  = EqProof Conjecture
  | SubstituteProof Conjecture Proof
  | UnfoldProof Conjecture Proof
  | SelectProof Conjecture Proof
  | SplitVarProof Conjecture [Proof]
  | InductionProof Conjecture [Proof]
  | SymmetryProof Conjecture Proof
  | ModusPonensProof Conjecture [Proof]
    deriving (Eq, Ord, Show)

instance Pretty Proof where
  pretty n (EqProof c) = (indent n $ pretty n c) ++ (indent n "@@@ REFLEXIVITY @@@")
  pretty n (UnfoldProof c p) =
    (indent n $ pretty n c) ++ (indent n "@@@ UNFOLD @@@") ++ (indent n $ pretty n p)
  pretty n (SelectProof c p) = (indent n $ pretty n c) ++ (indent n "@@@ SELECT @@@") ++ (pretty n p)
  pretty n (SplitVarProof c ps) = (indent n $ pretty n c) ++ (indent n "@@@ SPLIT @@@") ++ (L.concatMap (pretty (n+1)) ps)
  pretty n (InductionProof c ps) = (indent n $ pretty n c) ++ (indent n "@@@ INDUCTION @@@") ++ (L.concatMap (pretty (n+1)) ps)
  pretty n (SymmetryProof c p) = (indent n $ pretty n c) ++ (indent n "@@@ SYMMETRY @@@") ++ (pretty n p)
  pretty n (SubstituteProof c p) = (indent n $ pretty n c) ++ (indent n "@@@ SUBSTITUTE @@@") ++ (pretty n p)
  pretty n (ModusPonensProof c ps) = (indent n $ pretty n c) ++ (indent n "@@@ MODUS PONENS @@@") ++ (L.concatMap (pretty (n+1)) ps)

eqProof = EqProof
substituteProof = SubstituteProof
unfoldProof = UnfoldProof
selectProof = SelectProof
splitVarProof = SplitVarProof
inductionProof = InductionProof
symmetryProof = SymmetryProof
modusPonensProof = ModusPonensProof

depth (SubstituteProof _ sp) = 1 + depth sp
depth (SelectProof _ sp) = 1 + depth sp
depth (UnfoldProof _ sp) = 1 + depth sp
depth (SymmetryProof _ sp) = 1 + depth sp
depth (ModusPonensProof _ sps) = 1 + (L.maximum $ L.map (\sp -> 1 + depth sp) sps)
depth (InductionProof _ sps) = 1 + (L.maximum $ L.map (\sp -> 1 + depth sp) sps)
depth (SplitVarProof _ sps) = 1 + (L.maximum $ L.map (\sp -> 1 + depth sp) sps)
depth (EqProof _) = 1

subproofs :: Proof -> [Proof]
subproofs p@(SubstituteProof _ sp) = p:(subproofs sp)
subproofs p@(SelectProof _ sp) = p:(subproofs sp)
subproofs p@(UnfoldProof _ sp) = p:(subproofs sp)
subproofs p@(SymmetryProof _ sp) = p:(subproofs sp)
subproofs p@(ModusPonensProof _ sps) = p:(L.concatMap subproofs sps)
subproofs p@(InductionProof _ sps) = p:(L.concatMap subproofs sps)
subproofs p@(SplitVarProof _ sps) = p:(L.concatMap subproofs sps)
subproofs p@(EqProof _) = [p]
