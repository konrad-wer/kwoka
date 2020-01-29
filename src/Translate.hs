module Translate (translateProgram) where

import AST
import MachineAST
import qualified Data.Map as Map

binOpToMPrim :: String -> MPrim
binOpToMPrim "*"  = Mult
binOpToMPrim "/"  = Div
binOpToMPrim "%"  = Mod
binOpToMPrim "+"  = Add
binOpToMPrim "-"  = Sub
binOpToMPrim "^"  = Concat
binOpToMPrim ":"  = Cons
binOpToMPrim "@"  = Append
binOpToMPrim "==" = Equal
binOpToMPrim "!=" = NotEqual
binOpToMPrim "<=" = LessEqual
binOpToMPrim ">=" = GreaterEqual
binOpToMPrim "<"  = Less
binOpToMPrim ">"  = Greater
binOpToMPrim "&&" = And
binOpToMPrim "||" = Or
binOpToMPrim op = error ("Unknown operator: " ++ op)

mvalue :: MExpr -> Bool
mvalue (MBool _) = True
mvalue (MInt _) = True
mvalue (MString _) = True
mvalue _ = False

translate :: TypeEnv -> Expr p -> MExpr
translate _ (EVar _ x) = MVar x
translate _ (EBool _ b) = MBool  b
translate _ (EInt _ n) = MInt n
translate _ (EString _ s) = MString s
translate _ (ENil _) = MNil
translate c (EUnOp _ UnOpMinus e) =
  case translate c e of
    MInt n -> MInt (-n)
    m -> MPrim Neg $ MTuple [m]
translate c (EUnOp _ UnOpNot e) =
  case translate c e of
    MBool b -> MBool $ not b
    m -> MPrim Not $ MTuple [m]
translate c (EBinOp _ (BinOp op) e1 e2) =
  case (binOpToMPrim op, translate c e1, translate c e2) of
    (Mult, MInt n1, MInt n2) -> MInt $ n1 * n2
    (Div, MInt n1, MInt n2) -> MInt (n1 `div` n2)
    (Mod, MInt n1, MInt n2) -> MInt (n1 `mod` n2)
    (Add, MInt n1, MInt n2) -> MInt $ n1 + n2
    (Sub, MInt n1, MInt n2) -> MInt $ n1 - n2
    (And, MBool b1, MBool b2) -> MBool $ b1 && b2
    (Or, MBool b1, MBool b2) -> MBool $ b1 || b2
    (Concat, MString s1, MString s2) -> MString $ s1 ++ s2
    (Equal, m1, m2) | mvalue m1 && mvalue m2 -> MBool $ m1 == m2
    (NotEqual, m1, m2) | mvalue m1 && mvalue m2 -> MBool $ m1 /= m2
    (LessEqual, m1, m2) | mvalue m1 && mvalue m2 -> MBool $ m1 <= m2
    (GreaterEqual, m1, m2) | mvalue m1 && mvalue m2 -> MBool $ m1 >= m2
    (Less, m1, m2) | mvalue m1 && mvalue m2 -> MBool $ m1 < m2
    (Greater, m1, m2) | mvalue m1 && mvalue m2 -> MBool $ m1 > m2
    (mprim, m1, m2) -> MPrim mprim (MTuple [m1, m2])
translate c (ELambda _ args e) = MLambda args $ translate c e
translate c (EApp _ e1 e2) = MApp (translate c e1) (translate c e2)
translate c (EIf _ e0 e1 e2) = MIf (translate c e0) (translate c e1) (translate c e2)
translate c (ELet _ x e1 e2) = MApp (MLambda [x] $ translate c e2) $ MTuple [translate c e1]
translate c (ELetTuple _ xs e1 e2) = MApp (MLambda xs $ translate c e2) $ translate c e1
translate c (EOp _ name e) =
  let Just (TypeScheme _ (TArrow _ (EffLabel eff _) _)) = Map.lookup name c in
  MOp name eff $ translate c e
translate c (ETuple _ es) = MTuple $ map (translate c) es
translate c (ECase _ e0 e1 ptrn e2) = MCase (translate c e0) (translate c e1) ptrn (translate c e2)
translate c (EHandle _ name e clauses) = MHandle name (translate c e) $ translateClauses c clauses

translateClauses :: TypeEnv -> [Clause p] -> Map.Map Var MClause
translateClauses c = Map.fromList . map (\(Clause _ name args body) -> (name, MClause args $ translate c body))

translateProgram :: TypeEnv -> [FunDef p] -> (Maybe MExpr, RuntimeEnv)
translateProgram c =
  flip tp (Nothing, Map.empty)
  where
    tp [] acc = acc
    tp ((FunDef _ name args body) : funs) (_, env) =
      let env' = Map.insert name (VClosure env' args $ translate c body) env in
      tp funs (if name == "main" then Just . ioHandler $ translate c body else Nothing, env')

ioHandler :: MExpr -> MExpr
ioHandler e = MHandle "IO" e $ Map.fromList
  [("return", MClause ["x"] $ MVar "x"),
   ("GetLine", MClause [] $ MApp (MVar "resume") (MTuple [MPrim GetLine (MTuple [])])),
   ("ReadLnInt", MClause [] $ MApp (MVar "resume") (MTuple [MPrim ReadLnInt (MTuple [])])),
   ("PutStrLn", MClause ["s"] $ MApp (MVar "resume") (MTuple [MPrim Print (MTuple [MVar "s"])])),
   ("PrintInt", MClause ["x"] $ MApp (MVar "resume") (MTuple [MPrim Print (MTuple [MVar "x"])]))]