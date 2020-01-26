module AST where

import CommonUtils
import Data.List
import qualified Data.Map as Map

type Var = String

data TopLevelDef p = DefFun (FunDef p) | DefEff (EffectDef p)
data FunDef p = FunDef p Var [Var] (Expr p)
data EffectDef p = EffectDef p Var [ActionDef p]
data ActionDef p = ActionDef p Var [Type] (Maybe Type)

data UnOp = UnOpMinus | UnOpNot
newtype BinOp = BinOp String

data Expr p
  = EVar      p Var
  | EBool     p Bool
  | EInt      p Integer
  | EString   p String
  | ENil      p
  | EUnOp     p UnOp (Expr p)
  | EBinOp    p BinOp (Expr p) (Expr p)
  | ELambda   p [Var] (Expr p)
  | EApp      p (Expr p) (Expr p)
  | EIf       p (Expr p) (Expr p) (Expr p)
  | ELet      p Var (Expr p) (Expr p)
  | EAction   p Var (Expr p)
  | EHandle   p Var (Expr p) [Clause p]
  | ETuple    p [Expr p]
  | ELetTuple p [Var] (Expr p) (Expr p)
  | ECase     p (Expr p) (Expr p) (Var, Var) (Expr p)

data Clause p  = Clause p Var [Var] (Expr p)

type EffectEnv p = Map.Map String [ActionDef p]
type TypeEnv =  Map.Map Var TypeScheme

data TypeVar = T Var | E Var deriving (Eq, Ord)

data Type
  = TVar TypeVar
  | TBool
  | TInt
  | TString
  | TList Type
  | TProduct [Type]
  | TArrow Type EffectRow Type
  deriving Eq

data TypeScheme = TypeScheme [TypeVar] Type

data EffectRow
  = EffLabel Var EffectRow
  | EffEmpty
  | EffVar TypeVar
  deriving Eq

showIndent :: Int -> String
showIndent = flip replicate ' ' . (* 2)

showArgs :: [Var] -> String
showArgs = intercalate ", "

instance Show TypeVar where
  show (T a) = a
  show (E a) = a

instance Show Type where
  show (TVar a) = show a
  show TBool = "Bool"
  show TInt = "Int"
  show TString = "String"
  show (TList t) = addBrackets $ show t
  show (TProduct ts) = addParens $ intercalate ", " $ map show ts
  show (TArrow t1 effs t2) = addParens (show t1 ++ " -> " ++ show effs ++ " " ++ show t2)

instance Show EffectRow where
  show r = addAngles $ intercalate ", " $ toList r
    where
      toList EffEmpty = []
      toList (EffVar (E e)) = [e]
      toList (EffVar (T _)) = error "Internal complier error, effect var cannot be a T var"
      toList (EffLabel l es) = l : toList es

instance Show TypeScheme where
  show (TypeScheme [] t) = show t
  show (TypeScheme vars t) = "∀ " ++ intercalate ", " (map show vars) ++ " . " ++ show t

showProgram :: [TopLevelDef p] -> String
showProgram = intercalate "\n\n" . map show

showTypedProgram :: TypeEnv -> [TopLevelDef p] -> String
showTypedProgram c = intercalate "\n\n" . map (showTyped c)

showTyped :: TypeEnv -> TopLevelDef p -> String
showTyped _ (DefEff eff) = show eff
showTyped c (DefFun (FunDef _ name args e)) =
    "fn " ++ name ++ addParens (showArgs args) ++
    " // " ++ show (c Map.! name) ++
    "\n{\n" ++ showExpr 1 e ++ "\n}"

instance Show (TopLevelDef p) where
  show (DefFun f) = show f
  show (DefEff eff) = show eff

instance Show (EffectDef p) where
  show (EffectDef _ name actions) = "effect " ++ name ++
    "\n{" ++ (actions >>= ((("\n" ++ showIndent 1) ++) . show)) ++ "\n}"

instance Show (ActionDef p) where
  show (ActionDef _ name args annot) = name ++ "(" ++ intercalate "," (map show args) ++ ")" ++ showAnnot annot

instance Show (FunDef p) where
  show (FunDef _ name args e) =
    "fn " ++ name ++ addParens (showArgs args) ++
    "\n{\n" ++ showExpr 1 e ++ "\n}"

instance Show (Clause p) where
  show (Clause _ name args e) = name ++ addParens (intercalate ", " args) ++
    " => "  ++ show e

instance Show UnOp where
  show UnOpNot = "!"
  show UnOpMinus = "-"

instance Show BinOp where
  show (BinOp op) = op

showAnnot :: Maybe Type -> String
showAnnot Nothing = ""
showAnnot (Just t) = " :: " ++ show t

instance Show (Expr p) where
  show = showExpr 0

showExpr :: Int -> Expr p -> String
showExpr indent = (showIndent indent ++) . s indent
  where
    s _ (EVar _ x) = x
    s _ (ENil _) = "[]"
    s _ (EBool _ b) = show b
    s _ (EInt _ n) = show n
    s _ (EString _ str) = show str
    s _ (EUnOp _ op e) = show op ++ show e
    s i (EBinOp _ op e1 e2) = addParens $ s i e1 ++ " " ++ show op ++ " " ++ s i e2
    s i (ELambda _ args e) = "(fn " ++ addParens (showArgs args) ++ " => "++ s i e ++ ")"
    s i (EApp _ e1 e2) = s i e1 ++ s i e2
    s i (EIf _ e0 e1 e2) =
      "if " ++ s i e0 ++ " then\n" ++
        showIndent (i + 1) ++ s (i + 1) e1 ++ "\n" ++
      showIndent i ++ "else\n" ++
        showIndent (i + 1) ++ s (i + 1) e2
    s i (ECase _ e0 e1 (x, xs) e2) =
      "case " ++ s i e0 ++ " of\n" ++
      showIndent (i + 1) ++ "[] => " ++ s (i + 3) e1 ++ "\n" ++
      showIndent (i + 1) ++ x ++ " : " ++ xs ++ " => " ++ s (i + 3) e2
    s i (ELet _ x e1 e2) =
      "let " ++ x ++ " = " ++ s i e1 ++ " in\n" ++
      showIndent i ++ s i e2
    s i (ELetTuple _ xs e1 e2) =
      "let " ++ addParens (intercalate ", " xs) ++ " = " ++ s i e1 ++ " in\n" ++
      showIndent i ++ s i e2
    s i (EAction _ a e) = a ++ s i e
    s i (EHandle _ effName e cs) = "handle<" ++ effName ++ ">(" ++ s i e ++ ")\n" ++
      showIndent i ++ "{\n" ++
      intercalate ",\n" (sc (i + 1) <$> cs) ++ "\n" ++
      showIndent i ++ "}"
    s i (ETuple _ es) = addParens $ intercalate ", " $ map (s i) es
    sc i (Clause _ name args e) = showIndent i ++ name ++
      addParens (intercalate ", " args) ++ " => "  ++ s i e

getPos :: Expr p -> p
getPos (EVar      p _) = p
getPos (ENil      p) = p
getPos (EBool     p _) = p
getPos (EInt      p _) = p
getPos (EString   p _) = p
getPos (EUnOp     p _ _) = p
getPos (EBinOp    p _ _ _) = p
getPos (ELambda   p _ _) = p
getPos (EApp      p _ _) = p
getPos (EIf       p _ _ _) = p
getPos (ELet      p _ _ _) = p
getPos (ELetTuple p _ _ _) = p
getPos (EAction   p _ _) = p
getPos (EHandle   p _ _ _) = p
getPos (ETuple    p _) = p
getPos (ECase     p _ _ _ _) = p