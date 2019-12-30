{-# LANGUAGE  FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module TypeInference where

--Some ideas and solutions for implementation of the Hindley - Milner type system inspired by:
--http://dev.stephendiehl.com/fun/006_hindley_milner.html

import AST
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.State

data TypeError p

type TypeEnv =  Map.Map Var TypeScheme
type InferState p = StateT Int (Either (TypeError p))

type Subst = Map.Map Var Type

emptySubst :: Subst
emptySubst = Map.empty

compose :: Subst -> Subst -> Subst
compose s1 s2 = apply s1 s2 `Map.union` s1

class Substitutable t a where
  apply :: Map.Map Var t -> a -> a

instance Substitutable Type Type where
  apply _ TUnit   = TUnit
  apply _ TInt    = TInt
  apply _ TBool   = TBool
  apply _ TString = TString
  apply s (TArrow t1 eff t2) = TArrow (apply s t1) eff $ apply s t2
  apply s t@(TVar a) = fromMaybe t $ Map.lookup a s

instance Substitutable EffectRow EffectRow where
  apply _ EffEmpty = EffEmpty
  apply s (EffLabel _ er) = apply s er
  apply s e@(EffVar a) = fromMaybe e $ Map.lookup a s
  apply s EffWild = EffWild

instance Substitutable Type TypeScheme where
  apply s (TypeScheme as t) = TypeScheme as $ apply (foldr Map.delete s as) t

instance (Substitutable t a, Functor f, Foldable f) => Substitutable t (f a) where
  apply = fmap . apply

infer :: EffectEnv p -> TypeEnv -> Expr p -> InferState p Type
infer _ _ EUnit {}      = return TUnit
infer _ _ (EInt _ _)    = return TInt
infer _ _ (EBool _ _)   = return TBool
infer _ _ (EString _ _) = return TString
infer _ _ EBinOp {} = undefined
infer effs c (EUnOp _ UnOpNot e) = undefined
-- = EVar    p Var
-- | EUnOp   p UnOp (Expr p)
-- | EBinOp  p BinOp (Expr p) (Expr p)
-- | ELambda p [Arg] (Expr p)
-- | EApp    p (Expr p) (Expr p)
-- | EIf     p (Expr p) (Expr p) (Expr p)
-- | ELet    p Var (Maybe Type) (Expr p) (Expr p)
-- | EAction p Var (Expr p)
-- | EHandle p (Expr p) [Clause p]
-- | EAnnot  p (Expr p) Type
-- | ETuple  p [Expr p]

-- check :: EffectEnv p -> TypeEnv -> Expr p -> Type -> InferState p ()
-- check effs c e t = do
--   t' <- infer eff c e
--   unify

unify :: p -> Type -> Type -> InferState p Subst
unify _ TUnit TUnit = return emptySubst
unify _ TBool TBool = return emptySubst
unify _ TInt TInt = return emptySubst
unify _ TString TString = return emptySubst
unify p (TArrow t1 r t2) (TArrow t1' r' t2') = do
  rowEquivalence p r r'
  s <- unify p t1 t1'
  compose <$> unify p (apply s t2) (apply s t2') <*> pure s
unify p (TProduct ts1) (TProduct ts2) =
  foldM (\s (t1, t2) -> compose <$> unify p (apply s t1) (apply s t2) <*> pure s) emptySubst $ zip ts1 ts2
unify p (TVar a) (TVar b)
  | a == b = return emptySubst
  | otherwise = undefined

type RowSubst = Map.Map Var EffectRow

rowEquivalence :: p -> EffectRow -> EffectRow -> InferState p RowSubst
rowEquivalence = undefined