module TipParse() where

import Data.List as L
import Tip.Parser
import Tip.Parser.Convert
import Tip.Core
import Tip.Types

import Core

tipPath = "/Users/dillon/Haskell/ThmProving/tip/benchmarks/benchmarks/"
tipFile = "tip2015/nat_acc_plus_comm.smt2"

exPath = tipPath ++ tipFile

parseTipFile :: String -> IO (Either String Conjecture)
parseTipFile f = do
  parseRes <- parseFile f
  case parseRes of
   Left str -> return $ Left str
   Right th -> return $ convertTheory th

convertTheory :: Theory Id -> Either String Conjecture
convertTheory t = do
  dts <- sequence $ L.map convertDatatype $ thy_datatypes t
  funcs <- sequence $ L.map convertFunction $ thy_funcs t
  (assumptions, assertion) <- convertFormula $ thy_asserts t
  return $ conjecture dts funcs assumptions assertion

convertDatatype :: Tip.Core.Datatype Id -> Either String Core.Datatype
convertDatatype dt =
  case data_tvs dt of
   [] ->
     Right $ datatype (convId $ data_name dt) [] (L.map convertConstructor $ data_cons dt)
   _ -> Left "No support for type variables yet"

convertFunction :: Tip.Core.Function Id -> Either String Core.Function
convertFunction f =
  case func_tvs f of
   [] -> do
     b <- convExpr $ func_body f
     Right $ function (convId $ func_name f) [] (L.map convLocal $ func_args f) (convType $ func_res f) b
   _ -> Left "No support for type variables yet"

convertFormula :: [Tip.Core.Formula Id] -> Either String ([(Term, Term)], (Term, Term))
convertFormula dt =
  case dt of
   [] -> Left "No conjecture"
   [fs] -> convExprForm $ fm_body fs
   gs -> Left "More than 1 conjecture"

convExprForm e = do
  t <- discardUniversalQuantifiers e
  eres <- convExpr t
  Right ([], (eres, trueTermC))

discardUniversalQuantifiers (Quant _ Forall _ e) =
  discardUniversalQuantifiers e
discardUniversalQuantifiers (Quant _ Exists _ e) =
  Left "No support for existential quantifiers"
discardUniversalQuantifiers e = Right e

convId i = dId $ idString i

convertConstructor c = error "convertConstructor"

convType t = error "convType"

convExpr (h :@: es) = do
  g <- convHeadToGlobal h
  args <- sequence $ L.map convExpr es
  return $ ap g args

convHeadToGlobal (Gbl g) = Right $ convGlobal g
convHeadToGlobal (Builtin _) = Left "No support for builtins"

convGlobal g = error "convGlobal"
convLocal l = error "convLocal"
