{-# LANGUAGE  FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

module TypeInference where

--Some ideas and solutions for implementation of the Hindley - Milner type system inspired by:
--http://dev.stephendiehl.com/fun/006_hindley_milner.html

import AST
import CommonUtils
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List
import Control.Monad.State
import Text.Megaparsec.Pos

data TypeError p
  = TypesMismatchError p Type Type
  | UndeclaredVariableError p Var
  | UndeclaredActionError p Var
  | UndeclaredEffectError p Var
  | RowsNotEqualError p EffectRow EffectRow
  | NoLabelInClosedRowError p Var EffectRow
  | CyclicSubstInRowError p EffectRow EffectRow
  | CyclicSubstInTypeError p TypeVar Type
  | HandleActionsMismatchError p [Var] [Var]
  | ActionArityMismatchError p Int Int
  | DuplicateVarsInTuplePattern p [Var]
  | DuplicateVarsInListPattern p Var
  | DuplicateVarsInFunction p [Var]
  | ArgsForMainError p

instance SourcePos ~ p => Show (TypeError p) where
  show (TypesMismatchError p t1 t2) = sourcePosPretty p ++ "\nCouldn't match expected type " ++ addQuotes (show t1) ++
    " with actual type " ++ addQuotes (show t2)
  show (UndeclaredVariableError p x) = sourcePosPretty p ++ "\nVariable not in scope: " ++ addQuotes x
  show (UndeclaredActionError p a) = sourcePosPretty p ++ "\nUndefined action: " ++ addQuotes a
  show (UndeclaredEffectError p e) = sourcePosPretty p ++ "\nUndefined effect: " ++ addQuotes e
  show (RowsNotEqualError p r1 r2) = sourcePosPretty p ++ "\nCouldn't match effect row " ++ addQuotes (show r1) ++
    " with effect row " ++ addQuotes (show r2)
  show (NoLabelInClosedRowError p l r) = sourcePosPretty p ++ "\nCouldn't find effect label " ++ addQuotes (show l) ++
    " in the effect row " ++ addQuotes (show r)
  show (CyclicSubstInRowError p r1 r2) = sourcePosPretty p ++ "\nFound cyclic substitution while trying to unify effect row " ++
    addQuotes (show r1) ++ " with effect row " ++ addQuotes (show r2)
  show (CyclicSubstInTypeError p t1 t2) = sourcePosPretty p ++ "\nFound cyclic substitution while trying to unify type " ++
    addQuotes (show t1) ++ " with type " ++ addQuotes (show t2)
  show (HandleActionsMismatchError p actions clauses) =
    sourcePosPretty p ++ "\nCouldn't match expected action handlers: " ++ intercalate ", " (map addQuotes actions) ++
    " with actual action handlers: " ++  intercalate ", " (map addQuotes clauses)
  show (ActionArityMismatchError p n1 n2) = sourcePosPretty p ++ "\nCouldn't match expected action arity " ++ show n1 ++
    " with actual action handler arity " ++ show n2
  show (DuplicateVarsInTuplePattern p vars) = sourcePosPretty p ++ "\nDuplicate variable in tuple pattern: " ++
    (addQuotes . addParens . intercalate ", " $ vars)
  show (DuplicateVarsInListPattern p x) = sourcePosPretty p ++ "\nDuplicate variable in list pattern: " ++
    addQuotes (x ++ " : " ++ x)
  show (DuplicateVarsInFunction p vars) = sourcePosPretty p ++ "\nDuplicate variable in function arguments: " ++
    (addQuotes . intercalate ", " $ vars)
  show (ArgsForMainError p) = sourcePosPretty p ++ "\nThe main function should not take any arguments"

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
  apply s (TList t) = TList $ apply s t
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
  fv (TList t) = fv t
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

checkProgram :: EffectEnv p -> TypeEnv -> [FunDef p] -> Either (TypeError p) TypeEnv
checkProgram eff c funs =  flip evalStateT 0 $
  foldM (\cPrev fun@(FunDef _ name _ _) -> Map.insert name <$> inferFunDef eff cPrev fun <*> pure cPrev) c funs

inferFunDef :: EffectEnv p -> TypeEnv -> FunDef p -> InferState p TypeScheme
inferFunDef eff c (FunDef p name args body)
  | notUnique args = inferError $ DuplicateVarsInFunction p args
  | name == "main" && args /= [] = inferError $ ArgsForMainError p
  | otherwise = do
    ta <- mapM (const (TVar . T <$> freshVar)) args
    let c2 = foldr (uncurry Map.insert) c $ zip args $ map (TypeScheme []) ta
    tr <-  if name == "main" then return $ TProduct [] else TVar . T <$> freshVar
    r <-  if name == "main" then return (EffLabel "IO" EffEmpty) else EffVar . E <$> freshVar
    let t = TArrow (TProduct ta) r tr
    s <- check eff (Map.insert name (TypeScheme [] t) c2) body tr r
    return $ generalize (apply s c) $ apply s t

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
infer _ _ (ENil _) = (\x y -> (TList . TVar .T $ x, EffVar . E $ y, emptySubst)) <$> freshVar <*> freshVar
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
  s2 <- compose <$> check eff (apply s1 c) e2 (apply s1 ta) r' <*> pure s1
  s3 <- compose <$> unifyRow p (apply s2 tr) (apply s2 r) <*> pure s2
  s4 <- compose <$> unifyRow p (apply s3 r) (apply s3 r') <*> pure s3
  let res_t = apply s4 tt
  let res_r = apply s4 tr
  return (res_t, res_r, s4)
infer eff c (ETuple p elems) = do
  (ts, s1) <- inferElems eff emptySubst [] elems
  tr <- EffVar . E <$> freshVar
  (res_r, res_s) <- foldM (\(r1, sPrev) (_, r2) -> do
    s <- compose <$> unifyRow p (apply sPrev r1) (apply sPrev r2) <*> pure sPrev
    return (apply s r2, s)) (tr, s1) ts
  return (TProduct $ apply res_s $ map fst ts, res_r, res_s)
  where
    inferElems :: EffectEnv p -> Subst -> [(Type, EffectRow)] -> [Expr p] -> InferState p ([(Type, EffectRow)], Subst)
    inferElems _ sPrev acc [] = return (acc, sPrev)
    inferElems effEnv sPrev acc (e : es) = do
      (t, r, s) <- infer effEnv (apply sPrev c) e
      let s' = s `compose` sPrev
      inferElems effEnv s' (acc ++ [(apply s' t, apply s' r)]) es
infer eff c (EOp p name e) =
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
  (t, tr, s2) <- infer eff (apply s1 c) e1
  let s2' = s2 `compose` s1
  s3 <- compose <$> unifyRow p (apply s2' cr) (apply s2' tr) <*> pure s2'
  s4 <- compose <$> check eff (apply s3 c) e2 (apply s3 t) (apply s3 tr) <*> pure s3
  return (apply s4 t, apply s4 tr, s4)
infer eff c (ECase p e0 e1 (x, xs) e2)
  | x == xs = inferError $ DuplicateVarsInListPattern p x
  | otherwise = do
    r1 <- EffVar . E <$> freshVar
    tl <- TVar . T <$> freshVar
    s1 <- check eff c e0 (TList tl) r1
    (t, r2, s2) <- infer eff (apply s1 c) e1
    let s2' = s2 `compose` s1
    s3 <- compose <$> unifyRow p (apply s2' r1) (apply s2' r2) <*> pure s2'
    let c2 = apply s3 (Map.insert x (TypeScheme [] tl) (Map.insert xs (TypeScheme [] $ TList tl) c))
    s4 <- compose <$> check eff c2 e2 (apply s3 t) (apply s3 r2) <*> pure s3
    return (apply s4 t, apply s4 r2, s4)
infer eff c (ELet p x e1 e2) = do
  (tx, rx, s1) <- infer eff c e1
  let c' = apply s1 c
  let c2 = Map.insert x (generalize c' tx) c'
  (t, r, s2) <- infer eff c2 e2
  let s2' = s2 `compose` s1
  s3 <- compose <$> unifyRow p rx r <*> pure s2'
  return (apply s3 t, apply s3 r, s3)
infer eff c (ELetTuple p xs e1 e2)
  | notUnique xs = inferError $ DuplicateVarsInTuplePattern p xs
  | otherwise = do
    as <- mapM (const (TVar . T <$> freshVar)) xs
    rxs <- EffVar . E <$> freshVar
    s1 <- check eff c e1 (TProduct as) rxs
    let c' = apply s1 c
    let c2 = foldl (flip $ uncurry Map.insert) c' $ zip xs (map (generalize c' . apply s1) as)
    (t, r, s2) <- infer eff c2 e2
    let s2' = s2 `compose` s1
    s3 <- compose <$> unifyRow p rxs r <*> pure s2'
    return (apply s3 t, apply s3 r, s3)
infer eff c (EHandle p effName e clauses) =
  case Map.lookup effName eff of
    Nothing -> inferError $ UndeclaredEffectError p effName
    Just actions -> do
      let actionNames = sort $ "return" : map getActionName actions
      let clauseNames = sort $ map getClauseName clauses
      if actionNames /= clauseNames then
        inferError $ HandleActionsMismatchError p actionNames clauseNames
      else do
        (tx, r1, s1) <- infer eff c e
        (r2, s2) <- rewriteRow p effName (apply s1 r1)
        let s2' = s2 `compose` s1
        ts <- mapM (const (TVar . T <$> freshVar)) clauses
        rs <- mapM (const (EffVar . E <$> freshVar)) clauses
        t0 <- TVar . T <$> freshVar
        clsSubsts <- mapM (\(cl, tr, r) -> checkClause eff (apply s2 c) actions cl tx tr r) $ zip3 clauses ts rs
        let clsTypes = zipWith apply clsSubsts ts
        let clsRows = zipWith apply clsSubsts rs
        let clsPos = map (\(Clause pos _ _ _) -> pos) clauses
        let s3 = foldl compose s2' clsSubsts
        (tr, s4) <- foldM (\(tPrev, s) (t, pos) -> do
          s' <- compose <$> unify pos (apply s tPrev) (apply s t) <*> pure s
          return (apply s' t, s')) (t0, s3) $ zip clsTypes clsPos
        (r, s5) <- foldM (\(rPrev, s) (r, pos) -> do
          s' <- compose <$> unifyRow pos (apply s rPrev) (apply s r) <*> pure s
          return (apply s' r, s')) (r2, s4) $ zip clsRows clsPos
        return (apply s5 tr, apply s5 r, s5)
  where
    getActionName (ActionDef _ name _ _ ) = name
    getClauseName (Clause _ name _ _) = name

    checkClause :: EffectEnv p -> TypeEnv -> [ActionDef p] -> Clause p -> Type -> Type -> EffectRow -> InferState p Subst
    checkClause effEnv context actions (Clause _ name args ec) retArgType t r =
      case find ((==) name . getActionName) actions of
        Nothing -> check effEnv (Map.insert (head args) (TypeScheme [] retArgType) c) ec t r
        Just (ActionDef pos _ argTypes resumeType) -> do
          c2 <- extendEnv pos context args argTypes resumeType t
          check effEnv c2 ec t r

    extendEnv :: p -> TypeEnv -> [Var] -> [Type] -> Maybe Type -> Type ->  InferState p TypeEnv
    extendEnv pos context args argTypes resumeArgType retType
      | length args /= length argTypes =
        inferError $ ActionArityMismatchError pos (length argTypes) (length args)
      | otherwise = do
        let c2 = foldr (uncurry Map.insert) context $ zip args $ map (TypeScheme []) argTypes
        case resumeArgType of
          Nothing -> return c2
          Just rt -> return $ Map.insert "resume" (TypeScheme [] $ TArrow (TProduct [rt]) EffEmpty retType) c2

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
unify p (TList t1) (TList t2) = unify p t1 t2
unify p (TArrow t1 r t2) (TArrow t1' r' t2') = do
  s <- unify p t1 t1'
  s' <- compose <$> unifyRow p (apply s r) (apply s r') <*> pure s
  compose <$> unify p (apply s' t2) (apply s' t2') <*> pure s'
unify p (TProduct ts1) (TProduct ts2)
  | length ts1 /= length ts2 = inferError $ TypesMismatchError p (TProduct ts1) (TProduct ts2)
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
