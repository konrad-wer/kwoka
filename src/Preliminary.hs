{-# LANGUAGE GADTs #-}

module Preliminary where

import AST
import CommonUtils
import qualified Data.Map as Map
import Control.Monad.State
import Data.List
import Text.Megaparsec.Pos

data PreliminaryError p
  = DuplicateFunctionDefError p Var
  | DuplicateActionDefError p Var
  | DuplicateEffectDefError p Var

instance SourcePos ~ p => Show (PreliminaryError p) where
  show (DuplicateFunctionDefError p name) = sourcePosPretty p ++ "\nDuplicate definitions for function: " ++
    addQuotes name
  show (DuplicateActionDefError p name) = sourcePosPretty p ++ "\nDuplicate definitions for action: " ++
    addQuotes name
  show (DuplicateEffectDefError p name) = sourcePosPretty p ++ "\nDuplicate definitions for effect: " ++
    addQuotes name

buildEffectEnv :: [TopLevelDef p] -> Either (PreliminaryError p) (EffectEnv p)
buildEffectEnv [] = return Map.empty
buildEffectEnv (DefEff (EffectDef p name actions) : defs) = do
  tailRes <- buildEffectEnv defs
  case Map.lookup name tailRes of
    Just _ -> Left $ DuplicateEffectDefError p name
    Nothing -> return $ Map.insert name actions tailRes
buildEffectEnv (_ : defs) = buildEffectEnv defs

binOpTypes :: Map.Map Var TypeScheme
binOpTypes = Map.fromList
  [("*", TypeScheme [] $ TArrow (TProduct [TInt, TInt]) EffEmpty TInt),
   ("/", TypeScheme [] $ TArrow (TProduct [TInt, TInt]) EffEmpty TInt),
   ("%", TypeScheme [] $ TArrow (TProduct [TInt, TInt]) EffEmpty TInt),
   ("+", TypeScheme [] $ TArrow (TProduct [TInt, TInt]) EffEmpty TInt),
   ("-", TypeScheme [] $ TArrow (TProduct [TInt, TInt]) EffEmpty TInt),
   ("^", TypeScheme [] $ TArrow (TProduct [TString, TString]) EffEmpty TString),
   (":",  TypeScheme [T "%0"] $ TArrow (TProduct [TVar $ T "%0", TList $ TVar $ T "%0"]) EffEmpty (TList $ TVar $ T "%0")),
   ("@",  TypeScheme [T "%0"] $ TArrow (TProduct [TList . TVar $ T "%0", TList . TVar $ T "%0"]) EffEmpty (TList $ TVar $ T "%0")),
   ("==", TypeScheme [T "%1"] $ TArrow (TProduct [TVar $ T "%1", TVar $ T "%1"]) EffEmpty TBool),
   ("!=", TypeScheme [T "%2"] $ TArrow (TProduct [TVar $ T "%2", TVar $ T "%2"]) EffEmpty TBool),
   ("<=", TypeScheme [T "%3"] $ TArrow (TProduct [TVar $ T "%3", TVar $ T "%3"]) EffEmpty TBool),
   (">=", TypeScheme [T "%4"] $ TArrow (TProduct [TVar $ T "%4", TVar $ T "%4"]) EffEmpty TBool),
   ("<",  TypeScheme [T "%5"] $ TArrow (TProduct [TVar $ T "%5", TVar $ T "%5"]) EffEmpty TBool),
   (">",  TypeScheme [T "%6"] $ TArrow (TProduct [TVar $ T "%6", TVar $ T "%6"]) EffEmpty TBool),
   ("&&", TypeScheme [] $ TArrow (TProduct [TBool, TBool]) EffEmpty TBool),
   ("||", TypeScheme [] $ TArrow (TProduct [TBool, TBool]) EffEmpty TBool)]

buildTypeEnv :: [TopLevelDef p] -> Either (PreliminaryError p) TypeEnv
buildTypeEnv topLevelDefs =
  Map.union binOpTypes <$> evalStateT (bte topLevelDefs) 0
  where
    bte :: [TopLevelDef p] -> StateT Int (Either (PreliminaryError p)) TypeEnv
    bte [] = return Map.empty
    bte (DefEff (EffectDef p name actions) : defs) = do
      let names = map (\(ActionDef _ nm _ _) -> nm) actions
      ts <- mapM (buildActionType name) actions
      res <- bte defs
      foldM (\r (nm, t) ->
        if nm `Map.member` r then
          lift . Left $ DuplicateActionDefError p nm
        else
          return $ Map.insert nm t r) res $ zip names ts
    bte (_ : defs) = bte defs

    buildActionType :: Var -> ActionDef p -> StateT Int (Either (PreliminaryError p)) TypeScheme
    buildActionType effName (ActionDef _ _ args (Just tRes)) =
      return $ TypeScheme [] $ TArrow (TProduct args) (EffLabel effName EffEmpty) tRes
    buildActionType effName (ActionDef _ _ args Nothing) = do
      tRes <- T <$> freshVar
      return $ TypeScheme [tRes] $ TArrow (TProduct args) (EffLabel effName EffEmpty) $ TVar tRes

    freshVar :: StateT Int (Either (PreliminaryError p)) String
    freshVar = do
      n <- get
      modify (+1)
      return ("$" ++ show n)

getFuns :: [TopLevelDef p] -> Either (PreliminaryError p) [FunDef p]
getFuns defs =
  let funs = filterFuns defs in
  case filter ((> 1) . length) $ groupBy (\(FunDef _ n1 _ _) (FunDef _ n2 _ _) -> n1 == n2) funs of
    [] -> return funs
    ((FunDef p name _ _) : _) : _ -> Left $ DuplicateFunctionDefError p name
    _ -> undefined
  where
    filterFuns [] = []
    filterFuns (DefFun f : dfs) = f : filterFuns dfs
    filterFuns (_ : dfs) = filterFuns dfs

buildProgram :: [TopLevelDef p] -> Either (PreliminaryError p) ([FunDef p], EffectEnv p, TypeEnv)
buildProgram defs = do
  funs <- getFuns defs
  effectEnv <- buildEffectEnv defs
  typeEnv <- buildTypeEnv defs
  return (funs, effectEnv, typeEnv)
