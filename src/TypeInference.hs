{-# LANGUAGE  FlexibleInstances #-}

module TypeInference where

--Some ideas and solutions for implementation of the Hindley - Milner type system inspired by:
--http://dev.stephendiehl.com/fun/006_hindley_milner.html

import AST
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.State

data TypeError p
  = TypesMismatchError p Type Type
  | UndeclaredVariableError p Var
  | RowsNotEqualError p EffectRow EffectRow
  | NoLabelInClosedRowError p Var EffectRow
  | CyclicSubstInRowError p EffectRow EffectRow
  | CyclicSubstInTypeError p Var Type
  deriving (Show)

type TypeEnv =  Map.Map Var TypeScheme
type InferState p = StateT Int (Either (TypeError p))

data SubstObject
  = EffSubst EffectRow
  | TypeSubst Type
  deriving (Eq, Show)

inferError :: TypeError p -> InferState p a
inferError = lift . Left

type Subst = Map.Map Var SubstObject

freshVar :: InferState p String
freshVar = do
  n <- get
  modify (+1)
  return ("#" ++ show n)

emptySubst :: Subst
emptySubst = Map.empty

compose :: Subst -> Subst -> Subst
compose s1 s2 = apply s1 s2 `Map.union` s1

class Substitutable a where
  apply :: Subst -> a -> a
  fv    :: a -> Set.Set Var

instance Substitutable Type where
  apply _ TInt    = TInt
  apply _ TBool   = TBool
  apply _ TString = TString
  apply s (TArrow t1 eff t2) = TArrow (apply s t1) (apply s eff) $ apply s t2
  apply s (TProduct ts) = TProduct $ apply s ts
  apply s t@(TVar a) =
    case fromMaybe (TypeSubst t) $ Map.lookup a s of
      EffSubst eff -> error ("Internal complier error, while substitution in the type: " ++
                          show t ++ " for an effect row: " ++ show eff)
      TypeSubst t' -> t'

  fv TInt    = Set.empty
  fv TBool   = Set.empty
  fv TString = Set.empty
  fv (TArrow t1 r t2) = fv t1 `Set.union` fv r `Set.union` fv t2
  fv (TProduct ts) = foldr (Set.union . fv) Set.empty ts
  fv (TVar a) = Set.singleton a

instance Substitutable EffectRow where
  apply _ EffEmpty = EffEmpty
  apply s (EffLabel l er) = EffLabel l $ apply s er
  apply s eff@(EffVar a) =
    case fromMaybe (EffSubst eff) $ Map.lookup a s of
      EffSubst e' -> e'
      TypeSubst t -> error ("Internal complier error, while substitution in the effect row: " ++
                        show eff ++ " for an type: " ++ show t)

  fv EffEmpty = Set.empty
  fv (EffLabel _ r) = fv r
  fv (EffVar a) = Set.singleton a

instance Substitutable TypeScheme where
  apply s (TypeScheme as t) = TypeScheme as $ apply (foldr Map.delete s as) t
  fv (TypeScheme as t) = fv t `Set.difference` Set.fromList as

instance (Substitutable a, Functor f, Foldable f) => Substitutable (f a) where
  apply = fmap . apply
  fv = foldr (Set.union . fv) Set.empty

instance Substitutable SubstObject where
  apply s (EffSubst e) = EffSubst $ apply s e
  apply s (TypeSubst t) = TypeSubst $ apply s t
  fv (EffSubst e) = fv e
  fv (TypeSubst t) = fv t

infer :: EffectEnv p -> TypeEnv -> Expr p -> InferState p (Type, EffectRow, Subst)
infer _ c (EVar p x) =
  case Map.lookup x c of
    Nothing -> inferError $ UndeclaredVariableError p x
    Just scheme  -> do
      t <- instantiate scheme
      r <- EffVar <$> freshVar
      return  (t, r, emptySubst)
infer _ _ (EInt _ _)    = (\x -> (TInt, EffVar x, emptySubst)) <$> freshVar
infer _ _ (EBool _ _)   = (\x -> (TBool, EffVar x, emptySubst)) <$> freshVar
infer _ _ (EString _ _) = (\x -> (TString, EffVar x, emptySubst)) <$> freshVar
infer _ _ EBinOp {} = undefined
infer effs c (EUnOp _ UnOpNot e) = do
  r <- EffVar <$> freshVar
  s <- check effs c e TBool r
  return (TBool, r, s)
infer effs c (EUnOp _ UnOpMinus e) = do
  r <- EffVar <$> freshVar
  s <- check effs c e TInt r
  return (TInt, r, s)
infer eff c (ELambda _ args e) = do
  as <- mapM (const (TVar <$> freshVar)) args
  let c2 = foldr (uncurry Map.insert) c $ zip args $ map (TypeScheme []) as
  (tr, r, s) <- infer eff c2 e
  let ta = apply s as
  (\x -> (TArrow (TProduct ta) r tr, EffVar x, s)) <$> freshVar
infer eff c (EApp p e1 e2) = do
  ta <- TVar <$> freshVar
  tr <- EffVar <$> freshVar
  tt <- TVar <$> freshVar
  r <- EffVar <$> freshVar
  r' <- EffVar <$> freshVar
  s1 <- check eff c e1 (TArrow ta tr tt) r
  s2 <- check eff c e2 (apply s1 ta) r'
  s3 <- unifyRow p (apply (s2 `compose` s1 ) tr) (apply (s2 `compose` s1) r)
  s4 <- unifyRow p (apply (s3 `compose` s2 `compose` s1) r) (apply (s3 `compose` s2 `compose` s1) r')
  let res_s = s4 `compose` s3 `compose` s2 `compose` s1
  let res_t = apply res_s tt
  let res_r = apply res_s tr
  return (res_t, res_r, res_s)
infer eff c (ETuple p es) = do
  ts <- mapM (infer eff c) es
  tr <- EffVar  <$> freshVar
  (res_r, res_s) <- foldM (\(r1, s1) (_, r2, s2) -> do
    let s' = s2 `compose` s1
    s'' <- unifyRow p (apply s' r1) (apply s' r2)
    return (apply (s'' `compose` s') r2, s'' `compose` s')) (tr, emptySubst) ts
  return (TProduct $ apply res_s $ map (\(t, _, _) -> t) ts, res_r, res_s)


-- | EBinOp  p BinOp (Expr p) (Expr p)
-- | EIf     p (Expr p) (Expr p) (Expr p)
-- | ELet    p Var (Maybe Type) (Expr p) (Expr p)
-- | EAction p Var (Expr p)
-- | EHandle p (Expr p) [Clause p]
-- | ETuple  p [Expr p]

check :: EffectEnv p -> TypeEnv -> Expr p -> Type -> EffectRow -> InferState p Subst
check effs c e t r = do
  (t', r', s) <- infer effs c e
  s' <- compose <$> unify (getPos e) t t' <*> pure s
  compose <$> unifyRow (getPos e) (apply s' r) (apply s' r') <*> pure s'

instantiate :: TypeScheme -> InferState p Type
instantiate (TypeScheme as t) = do
  as' <- mapM (fmap (TypeSubst . TVar) . const freshVar) as
  let s = Map.fromList $ zip as as'
  return $ apply s t

unify :: p -> Type -> Type -> InferState p Subst
unify _ TBool TBool = return emptySubst
unify _ TInt TInt = return emptySubst
unify _ TString TString = return emptySubst
unify p (TArrow t1 r t2) (TArrow t1' r' t2') = do
  s <- unify p t1 t1'
  s' <- compose <$> unifyRow p (apply s r) (apply s r') <*> pure s
  compose <$> unify p (apply s' t2) (apply s' t2') <*> pure s'
unify p (TProduct ts1) (TProduct ts2) =
  foldM (\s (t1, t2) -> compose <$> unify p (apply s t1) (apply s t2) <*> pure s) emptySubst $ zip ts1 ts2
unify p (TVar a) t = bind p a t
unify p t (TVar a) = bind p a t
unify p t1 t2 = inferError $ TypesMismatchError p t1 t2

bind :: p -> Var -> Type -> InferState p Subst
bind p a t
  | t == TVar a = return emptySubst
  | a `Set.member` fv t = inferError $ CyclicSubstInTypeError p a t
  | otherwise = return $ Map.singleton a $ TypeSubst t

unifyRow :: p -> EffectRow -> EffectRow -> InferState p Subst
unifyRow _ EffEmpty EffEmpty = return emptySubst
unifyRow _ (EffVar x) (EffVar y)
  | x == y = return emptySubst
  | otherwise = return $ Map.singleton x $ EffSubst $ EffVar y
unifyRow p (EffVar x) e =
  if x `Set.member` fv e then
    inferError $ CyclicSubstInRowError p (EffVar x) e
  else
    return $ Map.singleton x $ EffSubst e
unifyRow _ EffEmpty (EffVar x) = return $ Map.singleton x $ EffSubst EffEmpty
unifyRow p r@EffLabel{} EffEmpty = inferError $ RowsNotEqualError p r EffEmpty
unifyRow p EffEmpty r@EffLabel{} = inferError $ RowsNotEqualError p EffEmpty r
unifyRow p (EffLabel l rt) r = do
  (rt', s) <- rewriteRow p l r
  if isCycle s $ rowTail rt then
    inferError $ CyclicSubstInRowError p (EffLabel l rt) r
  else do
    s' <- unifyRow p (apply s rt) (apply s rt')
    return $ compose s' s
  where
    rowTail EffEmpty = Nothing
    rowTail (EffVar a) = Just a
    rowTail (EffLabel _ row) = rowTail row

    isCycle s (Just x) = Map.member x s
    isCycle _ Nothing = False

--Since found label is moved always to the front of the row,
--we skip it in the result row and return only the tail part
rewriteRow :: p -> Var -> EffectRow -> InferState p (EffectRow, Subst)
rewriteRow p label row = do
  res <- rewrite [] label row
  case res of
    Nothing -> inferError $ NoLabelInClosedRowError p label row
    Just (r', s) -> return (r', s)
  where
    rewrite :: [Var] -> Var -> EffectRow -> InferState p (Maybe (EffectRow, Subst))
    rewrite _ _ EffEmpty = return Nothing
    rewrite acc l (EffLabel l' r)
      | l == l' = return $ Just (foldr EffLabel r acc, emptySubst)
      | otherwise = rewrite (acc ++ [l']) l r
    rewrite acc l (EffVar a) = do
      b <- EffVar <$> freshVar
      return $ Just (foldr EffLabel b acc, Map.singleton a (EffSubst $ EffLabel l b))

-- (EffLabel "a" . EffLabel "b"  . EffLabel "c"  . EffLabel "d" $ EffEmpty)