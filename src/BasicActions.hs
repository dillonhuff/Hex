module BasicActions(basicActions,
                    inductionAction,
                    eqAction,
                    unfoldAction, unfoldArgMatchedCallAction,
                    splitLocalAction,
                    substActionLHS, substActionRHS,
                    selectMatchAction,
                    symmetryAction) where

import Data.List as L

import Action
import Core
import Proof
import Utils

basicActions = [inductionAction,
                eqAction,
                substActionLHS,
                substActionRHS,
                selectMatchAction,
                unfoldAction,
                splitLocalAction,
                symmetryAction]

substActionLHS =
  action substTermLHS
substActionRHS =
  action substTermRHS
eqAction =
  action assumeEq
unfoldAction =
  action existsFunc
unfoldArgMatchedCallAction =
  action existsMatchedCallFunc
selectMatchAction =
  action existsDataConMatch
splitLocalAction =
  action existsLclSplit
inductionAction =
  action existsLcl
symmetryAction =
  applyIf (\c -> irreducible c (fst $ conjAssert c)) $ action (\c -> Just $ ([swapConj c], \[p] -> symmetryProof c p))

irreducible c t = isLcl t || isSaturatedConstructor c t

isSaturatedConstructor c t =
  isAp t && isConstructorCall c t && (L.and $ L.map isLcl $ callArgs t)

assumeEq c =
  let t0 = fst $ conjAssert c
      t1 = snd $ conjAssert c in
   case t0 == t1 of -- || (L.filter (\(a0, a1) -> (t0 == a0 && t1 == a1)) $ conjAssumptions c) /= [] of
    True -> Just ([], \_ -> eqProof c)
    False -> Nothing

substTermLHS c =
  case possibleSubstitutionsLHS c of
   [] -> Nothing
   subs ->
     let sub = L.head $ possibleSubstitutionsLHS c
         t1 = fst $ sub
         t2 = snd $ sub in
      Just ([c { conjAssert = (genSub (\t -> t == t1) (\t -> t2) $ fst $ conjAssert c, snd $ conjAssert c)}], \[s] -> substituteProof c s)

substTermRHS c =
  case possibleSubstitutionsRHS c of
   [] -> Nothing
   subs ->
     let sub = L.head $ possibleSubstitutionsRHS c
         t1 = fst $ sub
         t2 = snd $ sub in
      Just ([c { conjAssert = (fst $ conjAssert c, genSub (\t -> t == t1) (\t -> t2) $ snd $ conjAssert c)}], \[s] -> substituteProof c s)

possibleSubstitutionsLHS c =
  let as = conjAssumptions c
      lhs = fst $ conjAssert c in
   [(s1, s2) | (s1, s2) <- as, existsTerm (\t -> t == s1) lhs]

possibleSubstitutionsRHS c =
  let as = conjAssumptions c
      rhs = snd $ conjAssert c in
   [(s1, s2) | (s1, s2) <- as, existsTerm (\t -> t == s1) rhs]

existsLcl c =
  case collectLcls $ fst $ conjAssert c of
   [] -> Nothing
   lcls -> Just (inductionLcl c, inductionProof c)

inductionLcl c =
  let lclToInd = L.head $ collectLcls $ fst $ conjAssert c
      cons = lookupConstructors (lclType $ getLocal lclToInd) c in
   L.map (inductionSubgoal lclToInd c) cons

inductionSubgoal l c con =
  let k = freshConstructorCall (fst $ conjAssert c) con
      fvs = callArgs k
      recVars = L.filter (\v -> (lclType $ getLocal v) == (lclType $ getLocal l)) fvs
      oldAssert = fst $ conjAssert c
      newAssert = genSub (\t -> t == l) (\t -> k) oldAssert
      newAssertR = genSub (\t -> t == l) (\t -> k) $ snd $ conjAssert c
      newAssumptions = L.map (\x -> (genSub (\t -> t == l) (\t -> x) oldAssert, genSub (\t -> t == l) (\t -> x) $ snd $ conjAssert c)) fvs in
   c { conjAssumptions = newAssumptions ++ (conjAssumptions c),
       conjAssert = (newAssert, newAssertR) }

existsLclSplit c =
  case collectLcls $ fst $ conjAssert c of
   [] -> Nothing
   lcls -> Just (splitLcl c, splitVarProof c)

splitLcl c =
  let lclToSplit = L.head $ collectLcls $ fst $ conjAssert c
      cons = lookupConstructors (lclType $ getLocal lclToSplit) c in
   L.map (addSplitAssumption lclToSplit c) cons

addSplitAssumption lcl c con =
  c { conjAssert = (genSub (\t -> t == lcl) (\t -> freshConstructorCall (fst $ conjAssert c) con) (fst $ conjAssert c), snd $ conjAssert c) }

freshConstructorCall t con =
  let fvs = freshVars t (dcArgs con) in
   ap (dcName con) fvs

freshVars t args =
  let n = nextTempIndex t in
   L.zipWith (\t i -> lcl $ dLcl (tempVarName i) t) args [n..(n + (L.length args) - 1)]

nextTempIndex t =
  let tmps = collectFromTerms (\t -> if isTemp t then [t] else []) t
      tmpVals = L.map (\tmp -> read $ L.drop 1 $ idName $ lclName $ getLocal tmp) tmps in
   if tmpVals == [] then 0 else (L.maximum tmpVals) + 1

isTemp t = isLcl t && (L.head $ idName $ lclName $ getLocal t) == '$'

tempVarName i = "$" ++ show i

lookupConstructors t c =
  let tName = tyConName t
      dts = conjDataTypes c in
   dtConstructors $ L.head $ L.filter (\dt -> dtName dt == tName) dts

collectLcls tm = collectFromTerms (\t -> if isLcl t then [t] else []) tm

existsDataConMatch c =
  case existsTerm (isDataConMatch c) $ fst $ conjAssert c of
   True -> Just (substituteDataConMatches c, simpleSelectProof c)
   False -> Nothing

substituteDataConMatches c =
  [c {conjAssert = (genSub (isDataConMatch c) selectMatch $ fst $ conjAssert c, snd $ conjAssert c)}]

existsMatchedCallFunc c =
  let a = fst $ conjAssert c in
   case collectFromTerms (\t -> if isMatchedCall t c then [t] else []) a of
    (t:_) ->
      let f = getCalledFunc t c in
       Just (subFunc f c, simpleUnfoldProof c)
    _ -> Nothing

getCalledFunc t c =
  let name = gblName $ callHead t in
   L.head $ L.filter (\f -> funcName f == name) $ conjFunctions c

isMatchedCall t c =
  case isFuncall c t of
   True ->
     let body = funcBody $ getCalledFunc t c
         params = callArgs t
         argVars = funcArgs $ getCalledFunc t c
         matchedTerms = collectFromTerms (\e -> if isMatch e then [matchedTerm e] else []) body in
      L.or $ L.zipWith (\v e -> isDataCon c e && L.elem (lcl v) matchedTerms) argVars params
   False -> False

subFunc f c =
  [c { conjAssert = (replaceFuncWithBody f $ fst $ conjAssert c, snd $ conjAssert c)}]

existsFunc c =
  case existsTerm (isFuncall c) $ fst $ conjAssert c of
   True -> Just (substituteFunc c, simpleUnfoldProof c)
   False -> Nothing

substituteFunc c =
  case conjFunctions c of
   (f:fs) ->
      [c { conjAssert = (replaceFuncWithBody f $ fst $ conjAssert c, snd $ conjAssert c)}]
   _ -> [c]

simpleUnfoldProof c [subProof] = unfoldProof c subProof
simpleSelectProof c [subProof] = selectProof c subProof

replaceFuncWithBody f =
  let fName = funcName f
      fBody = funcBody f
      fArgs = funcArgs f in
   genSub (sameFunc fName) (replaceFunc fBody fArgs)

firstNoArgFunc [] = Nothing
firstNoArgFunc (f:fs) =
  if funcArgs f == [] then Just f else firstNoArgFunc fs
