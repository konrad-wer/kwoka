module ASTBuilder where

import AST
import qualified Data.Map as Map
import Control.Monad.State

buildEffectEnv :: [TopLevelDef p] -> EffectEnv p
buildEffectEnv [] = Map.empty
buildEffectEnv (DefEff (EffectDef _ name actions) : defs) = Map.insert name actions $ buildEffectEnv defs
buildEffectEnv (_ : defs) = buildEffectEnv defs

binOpTypes :: Map.Map Var TypeScheme
binOpTypes = Map.fromList
  [("*", TypeScheme [] $ TArrow (TProduct [TInt, TInt]) EffEmpty TInt),
   ("/", TypeScheme [] $ TArrow (TProduct [TInt, TInt]) EffEmpty TInt),
   ("%", TypeScheme [] $ TArrow (TProduct [TInt, TInt]) EffEmpty TInt),
   ("+", TypeScheme [] $ TArrow (TProduct [TInt, TInt]) EffEmpty TInt),
   ("-", TypeScheme [] $ TArrow (TProduct [TInt, TInt]) EffEmpty TInt),
   ("^", TypeScheme [] $ TArrow (TProduct [TString, TString]) EffEmpty TString),
   ("==", TypeScheme [T "%1"] $ TArrow (TProduct [TVar $ T "%1", TVar $ T "%1"]) EffEmpty TBool),
   ("!=", TypeScheme [T "%2"] $ TArrow (TProduct [TVar $ T "%2", TVar $ T "%2"]) EffEmpty TBool),
   ("<=", TypeScheme [T "%3"] $ TArrow (TProduct [TVar $ T "%3", TVar $ T "%3"]) EffEmpty TBool),
   (">=", TypeScheme [T "%4"] $ TArrow (TProduct [TVar $ T "%4", TVar $ T "%4"]) EffEmpty TBool),
   ("<",  TypeScheme [T "%5"] $ TArrow (TProduct [TVar $ T "%5", TVar $ T "%5"]) EffEmpty TBool),
   (">",  TypeScheme [T "%6"] $ TArrow (TProduct [TVar $ T "%6", TVar $ T "%6"]) EffEmpty TBool),
   ("&&", TypeScheme [] $ TArrow (TProduct [TBool, TBool]) EffEmpty TBool),
   ("||", TypeScheme [] $ TArrow (TProduct [TBool, TBool]) EffEmpty TBool)]

buildTypeEnv :: [TopLevelDef p] -> TypeEnv
buildTypeEnv topLevelDefs =
  Map.union binOpTypes $ flip evalState 0 $ bte topLevelDefs
  where
    bte :: [TopLevelDef p] -> State Int TypeEnv
    bte [] = return Map.empty
    bte (DefEff (EffectDef _ name actions) : defs) = do
      let names = map (\(ActionDef _ nm _ _) -> nm) actions
      ts <- mapM (buildActionType name) actions
      res <- bte defs
      return $ foldr (uncurry Map.insert) res $ zip names ts
    bte (_ : defs) = bte defs

    buildActionType :: Var -> ActionDef p -> State Int TypeScheme
    buildActionType effName (ActionDef _ _ args (Just tRes)) =
      return $ TypeScheme [] $ TArrow (TProduct args) (EffLabel effName EffEmpty) tRes
    buildActionType effName (ActionDef _ _ args Nothing) = do
      tRes <- T <$> freshVar
      return $ TypeScheme [tRes] $ TArrow (TProduct args) (EffLabel effName EffEmpty) $ TVar tRes

    freshVar :: State Int String
    freshVar = do
      n <- get
      modify (+1)
      return ("$" ++ show n)

getFuns :: [TopLevelDef p] -> [FunDef p]
getFuns [] = []
getFuns (DefFun f : defs) = f : getFuns defs
getFuns (_ : defs) = getFuns defs