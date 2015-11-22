module Proof(Proof,
             trueProof, eqProof, substituteProof, unfoldProof,
             selectProof, splitVarProof, inductionProof,
             Conjecture) where

import Data.List as L

import Core
import Utils

data Proof
  = TrueProof Conjecture
  | EqProof Conjecture
  | SubstituteProof Conjecture Proof
  | UnfoldProof Conjecture Proof
  | SelectProof Conjecture Proof
  | SplitVarProof Conjecture [Proof]
  | InductionProof Conjecture [Proof]
    deriving (Eq, Ord, Show)

instance Pretty Proof where
  pretty n (TrueProof c) = (indent n $ pretty n c) ++ (indent n "@@@ TRUE @@@")
  pretty n (EqProof c) = (indent n $ pretty n c) ++ (indent n "@@@ ASSUMPTION @@@")
  pretty n (UnfoldProof c p) =
    (indent n $ pretty n c) ++ (indent n "@@@ UNFOLD @@@") ++ (indent n $ pretty n p)
  pretty n (SelectProof c p) = (indent n $ pretty n c) ++ (indent n "@@@ SELECT @@@") ++ (pretty n p)
  pretty n (SplitVarProof c ps) = (indent n $ pretty n c) ++ (indent n "@@@ SPLIT @@@") ++ (L.concatMap (pretty (n+1)) ps)
  pretty n (InductionProof c ps) = (indent n $ pretty n c) ++ (indent n "@@@ INDUCTION @@@") ++ (L.concatMap (pretty (n+1)) ps)

trueProof = TrueProof
eqProof = EqProof
substituteProof = SubstituteProof
unfoldProof = UnfoldProof
selectProof = SelectProof
splitVarProof = SplitVarProof
inductionProof = InductionProof
