module ASTBuilder where

import AST
import qualified Data.Map as Map
import Control.Monad.State

buildEffectEnv :: [TopLevelDef p] -> Map.Map String [ActionDef p]
buildEffectEnv [] = Map.empty
buildEffectEnv (DefEff (EffectDef _ name actions) : defs) = Map.insert name actions $ buildEffectEnv defs
buildEffectEnv (_ : defs) = buildEffectEnv defs

binOpTypes :: Map.Map Var TypeScheme
binOpTypes = Map.fromList
  [("*", TypeScheme [E "%1"] $ TArrow (TProduct [TInt, TInt]) (EffVar $ E "%1") TInt),
   ("/", TypeScheme [E "%2"] $ TArrow (TProduct [TInt, TInt]) (EffVar $ E "%2") TInt),
   ("%", TypeScheme [E "%3"] $ TArrow (TProduct [TInt, TInt]) (EffVar $ E "%3") TInt),
   ("+", TypeScheme [E "%4"] $ TArrow (TProduct [TInt, TInt]) (EffVar $ E "%4") TInt),
   ("-", TypeScheme [E "%5"] $ TArrow (TProduct [TInt, TInt]) (EffVar $ E "%5") TInt),
   ("==", TypeScheme [T "%6", E "%7"] $ TArrow (TProduct [TVar $ T "%6", TVar $ T "%6"]) (EffVar $ E "%7") TBool),
   ("!=", TypeScheme [T "%8", E "%9"] $ TArrow (TProduct [TVar $ T "%8", TVar $ T "%8"]) (EffVar $ E "%9") TBool),
   ("<=", TypeScheme [T "%10", E "%11"] $ TArrow (TProduct [TVar $ T "%10", TVar $ T "%10"]) (EffVar $ E "%11") TBool),
   (">=", TypeScheme [T "%12", E "%13"] $ TArrow (TProduct [TVar $ T "%12", TVar $ T "%12"]) (EffVar $ E "%13") TBool),
   ("<",  TypeScheme [T "%14", E "%15"] $ TArrow (TProduct [TVar $ T "%14", TVar $ T "%14"]) (EffVar $ E "%15") TBool),
   (">",  TypeScheme [T "%16", E "%17"] $ TArrow (TProduct [TVar $ T "%16", TVar $ T "%16"]) (EffVar $ E "%17") TBool),
   ("&&", TypeScheme [E "%18"] $ TArrow (TProduct [TBool, TBool]) (EffVar $ E "%18") TBool),
   ("||", TypeScheme [E "%19"] $ TArrow (TProduct [TBool, TBool]) (EffVar $ E "%19") TBool)]

buildTypeEnv :: [TopLevelDef p] -> Map.Map String TypeScheme
buildTypeEnv topLevelDefs =
  Map.union binOpTypes $ flip evalState 0 $ bte topLevelDefs
  where
    bte :: [TopLevelDef p] -> State Int (Map.Map String TypeScheme)
    bte [] = return Map.empty
    bte (DefEff (EffectDef _ name actions) : defs) = do
      let names = map (\(ActionDef _ nm _) -> nm) actions
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