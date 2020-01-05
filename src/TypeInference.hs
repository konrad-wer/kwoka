{-# LANGUAGE  FlexibleInstances #-}

module TypeInference where

--Some ideas and solutions for implementation of the Hindley - Milner type system inspired by:
--http://dev.stephendiehl.com/fun/006_hindley_milner.html

import AST
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List
import Control.Monad.State

data TypeError p
  = TypesMismatchError p Type Type
  | UndeclaredVariableError p Var
  | UndeclaredActionError p Var
  | UndeclaredEffectError p Var
  | RowsNotEqualError p EffectRow EffectRow
  | NoLabelInClosedRowError p Var EffectRow
  | CyclicSubstInRowError p EffectRow EffectRow
  | CyclicSubstInTypeError p TypeVar Type
  | ProductArityMismatchError p Type Type
  | HandleActionsMismatchError p [Var] [Var]
  | ActionArityMismatchError p Int Int
  deriving (Show)

type TypeEnv =  Map.Map Var TypeScheme
type InferState p = StateT Int (Either (TypeError p))

data SubstObject
  = EffSubst EffectRow
  | TypeSubst Type
  deriving (Eq, Show)

inferError :: TypeError p -> InferState p a
inferError = lift . Left

type Subst = Map.Map TypeVar SubstObject

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
  fv    :: a -> Set.Set TypeVar

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
    Just scheme -> do
      t <- instantiate scheme
      r <- EffVar . E <$> freshVar
      return  (t, r, emptySubst)
infer _ _ (EInt _ _)    = (\x -> (TInt, EffVar . E $ x, emptySubst)) <$> freshVar
infer _ _ (EBool _ _)   = (\x -> (TBool, EffVar . E $ x, emptySubst)) <$> freshVar
infer _ _ (EString _ _) = (\x -> (TString, EffVar . E $ x, emptySubst)) <$> freshVar
infer eff c (EBinOp p (BinOp op) e1 e2) =
  infer eff c (EApp p (EVar p op) (ETuple p [e1, e2]))
infer effs c (EUnOp _ UnOpNot e) = do
  r <- EffVar . E <$> freshVar
  s <- check effs c e TBool r
  return (TBool, r, s)
infer effs c (EUnOp _ UnOpMinus e) = do
  r <- EffVar . E <$> freshVar
  s <- check effs c e TInt r
  return (TInt, r, s)
infer eff c (ELambda _ args e) = do
  as <- mapM (const (TVar . T <$> freshVar)) args
  let c2 = foldr (uncurry Map.insert) c $ zip args $ map (TypeScheme []) as
  (tr, r, s) <- infer eff c2 e
  let ta = apply s as
  (\x -> (TArrow (TProduct ta) r tr, EffVar . E $ x, s)) <$> freshVar
infer eff c (EApp p e1 e2) = do
  ta <- TVar . T <$> freshVar
  tr <- EffVar . E <$> freshVar
  tt <- TVar . T <$> freshVar
  r <- EffVar . E <$> freshVar
  r' <- EffVar . E <$> freshVar
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
  tr <- EffVar . E <$> freshVar
  (res_r, res_s) <- foldM (\(r1, s1) (_, r2, s2) -> do
    let s' = s2 `compose` s1
    s'' <- compose <$> unifyRow p (apply s' r1) (apply s' r2) <*> pure s'
    return (apply s'' r2, s'')) (tr, emptySubst) ts
  return (TProduct $ apply res_s $ map (\(t, _, _) -> t) ts, res_r, res_s)
infer eff c (EAction p name e) =
  case Map.lookup name c of
    Nothing -> inferError $ UndeclaredActionError p name
    Just scheme -> do
      t <- instantiate scheme
      case t of
        TArrow ta r tr -> do
          r' <- EffVar . E <$> freshVar
          s <- check eff c e ta r'
          s' <- unifyRow p (apply s r) (apply s r')
          let s'' = s' `compose` s
          return (apply s'' tr, apply s'' r, s'')
        _ -> error "Internal complier error, actions should have type TArrow, something went totally wrong"
infer eff c (EIf p e0 e1 e2) = do
  cr <- EffVar . E <$> freshVar
  s1 <- check eff c e0 TBool cr
  (t, tr, s2) <- infer eff c e1
  let s2' = s2 `compose` s1
  s3 <- compose <$> unifyRow p (apply s2' cr) (apply s2' tr) <*> pure s2'
  s4 <- compose <$> check eff c e2 (apply s3 t) (apply s3 tr) <*> pure s3
  return (apply s4 t, apply s4 tr, s4)
infer eff c (ELet p x e1 e2) = do
  (tx, rx, s1) <- infer eff c e1
  let c' = apply s1 c
  let c2 = Map.insert x (generalize c' tx) c'
  (t, r, s2) <- infer eff c2 e2
  let s2' = s2 `compose` s1
  s3 <- compose <$> unifyRow p rx r <*> pure s2'
  return (apply s3 t, apply s3 r, s3)
infer eff c (EHandle p effName e clauses) =
  case Map.lookup effName eff of
    Nothing -> inferError $ UndeclaredEffectError p effName
    Just actions -> do
      let actionNames = List.sort $ "return" : map getActionName actions
      let clauseNames = List.sort $ map getClauseName clauses
      if actionNames /= clauseNames then
        inferError $ HandleActionsMismatchError p actionNames clauseNames
      else do
        (tx, r1, s1) <- infer eff c e
        (r2, s2) <- rewriteRow p effName (apply s1 r1)
        let s2' = s2 `compose` s1
        ts <- mapM (const (TVar . T <$> freshVar)) clauses
        rs <- mapM (const (EffVar . E <$> freshVar)) clauses
        t0 <- TVar . T <$> freshVar
        r0 <- EffVar . E <$> freshVar
        clsSubsts <- mapM (\(cl, tr, r) -> checkClause p eff actions cl tx tr r) $ zip3 clauses ts rs
        let clsTypes = zipWith apply clsSubsts ts
        let clsRows = zipWith apply clsSubsts rs
        let s3 = foldl1 compose clsSubsts
        (tr, s4) <- foldM (\(tPrev, s) t -> do
          s' <- compose <$> unify p (apply s tPrev) (apply s t) <*> pure s
          return (apply s' t, s')) (t0, s3) clsTypes
        (r, s5) <- foldM (\(rPrev, s) r -> do
          s' <- compose <$> unifyRow p (apply s rPrev) (apply s r) <*> pure s
          return (apply s' r, s')) (r0, s4) clsRows
        return (apply s5 tr, apply s5 r, s5)
  where
    getActionName (ActionDef _ name _ _ ) = name
    getClauseName (Clause _ name _ _) = name

    checkClause :: p -> EffectEnv p -> [ActionDef p] -> Clause p -> Type -> Type -> EffectRow -> InferState p Subst
    checkClause pos effEnv actions (Clause _ name args e) retArgType t r =
      case List.find ((==) name . getActionName) actions of
        Nothing -> check effEnv (Map.insert (head args) (TypeScheme [] retArgType) c) e t r
        Just (ActionDef _ _ argTypes resumeType) -> do
          c2 <- extendEnv pos c args argTypes resumeType t
          check effEnv c2 e t r

    extendEnv :: p -> TypeEnv -> [Var] -> [Type] -> Maybe Type -> Type ->  InferState p TypeEnv
    extendEnv pos context args argTypes resumeArgType retType
      | length args /= length argTypes =
        inferError $ ActionArityMismatchError pos (length argTypes) (length args)
      | otherwise = do
        let c2 = foldr (uncurry Map.insert) context $ zip args $ map (TypeScheme []) argTypes
        case resumeArgType of
          Nothing -> return c2
          Just rt -> return $ Map.insert "resume" (TypeScheme [] $ TArrow rt EffEmpty retType) c2


check :: EffectEnv p -> TypeEnv -> Expr p -> Type -> EffectRow -> InferState p Subst
check effs c e t r = do
  (t', r', s) <- infer effs c e
  s' <- compose <$> unify (getPos e) t t' <*> pure s
  compose <$> unifyRow (getPos e) (apply s' r) (apply s' r') <*> pure s'

instantiate :: TypeScheme -> InferState p Type
instantiate (TypeScheme as t) = do
  as' <- mapM aux as
  let s = Map.fromList $ zip as as'
  apply s <$> open t
    where
      aux (T _) = TypeSubst . TVar . T <$> freshVar
      aux (E _) = EffSubst . EffVar . E <$> freshVar

open :: Type -> InferState p Type
open (TArrow t1 r t2) = TArrow <$> open t1 <*> openRow r <*> open t2
open t = return t

openRow :: EffectRow -> InferState p EffectRow
openRow EffEmpty = EffVar . E <$> freshVar
openRow (EffLabel l r) = EffLabel l <$> openRow r
openRow r = return r

generalize :: TypeEnv -> Type -> TypeScheme
generalize c t = TypeScheme (Set.elems $ fv (close t) `Set.difference` fv c) (close t)

close :: Type -> Type
close (TArrow t1 r t2) = TArrow (close t1) (closeRow r) (close t2)
close t = t

closeRow :: EffectRow -> EffectRow
closeRow (EffVar _) = EffEmpty
closeRow (EffLabel l r) = EffLabel l $ closeRow r
closeRow r = r

unify :: p -> Type -> Type -> InferState p Subst
unify _ TBool TBool = return emptySubst
unify _ TInt TInt = return emptySubst
unify _ TString TString = return emptySubst
unify p (TArrow t1 r t2) (TArrow t1' r' t2') = do
  s <- unify p t1 t1'
  s' <- compose <$> unifyRow p (apply s r) (apply s r') <*> pure s
  compose <$> unify p (apply s' t2) (apply s' t2') <*> pure s'
unify p (TProduct ts1) (TProduct ts2)
  | length ts1 /= length ts2 = inferError $ ProductArityMismatchError p (TProduct ts1) (TProduct ts2)
  | otherwise =
    foldM (\s (t1, t2) -> compose <$> unify p (apply s t1) (apply s t2) <*> pure s) emptySubst $ zip ts1 ts2
unify p (TVar a) t = bind p a t
unify p t (TVar a) = bind p a t
unify p t1 t2 = inferError $ TypesMismatchError p t1 t2

bind :: p -> TypeVar -> Type -> InferState p Subst
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
      b <- EffVar . E <$> freshVar
      return $ Just (foldr EffLabel b acc, Map.singleton a (EffSubst $ EffLabel l b))
