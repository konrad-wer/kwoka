module ASTBuilder where

import AST
import qualified Data.Map as Map
import Control.Monad.State

buildEffectEnv :: [TopLevelDef p] -> Map.Map String [ActionDef p]
buildEffectEnv [] = Map.empty
buildEffectEnv (DefEff (EffectDef _ name actions) : defs) = Map.insert name actions $ buildEffectEnv defs
buildEffectEnv (_ : defs) = buildEffectEnv defs

buildTypeEnv :: [TopLevelDef p] -> Map.Map String TypeScheme
buildTypeEnv topLevelDefs =
  flip evalState 0 $ bte topLevelDefs
  where
    bte :: [TopLevelDef p] -> State Int (Map.Map String TypeScheme)
    bte [] = return Map.empty
    bte (DefEff (EffectDef _ name actions) : defs) = do
      let names = map (\(ActionDef _ name _) -> name) actions
      ts <- mapM (buildActionType name) actions
      res <- bte defs
      return $ foldr (uncurry Map.insert) res $ zip names ts
    bte (_ : defs) = bte defs

    buildActionType :: Var -> ActionDef p -> State Int TypeScheme
    buildActionType effName (ActionDef _ _ args) = do
      tArgs <- mapM (const (T <$> freshVar)) args
      tRow <- E <$> freshVar
      tRes <- T <$> freshVar
      return $ TypeScheme (tArgs ++ [tRow] ++ [tRes]) $
        TArrow (TProduct $ map TVar tArgs) (EffLabel effName $ EffVar tRow) $ TVar tRes

    freshVar :: State Int String
    freshVar = do
      n <- get
      modify (+1)
      return ("$" ++ show n)