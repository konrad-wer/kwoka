module Translate (translateProgram) where

import AST
import MachineAST
import CommonUtils
import qualified Data.Map as Map

binOpToMOp :: String ->  MOp
binOpToMOp "*"  = Mult
binOpToMOp "/"  = Div
binOpToMOp "%"  = Mod
binOpToMOp "+"  = Add
binOpToMOp "-"  = Sub
binOpToMOp "^"  = Concat
binOpToMOp ":"  = Cons
binOpToMOp "@"  = Append
binOpToMOp "==" = Equal
binOpToMOp "!=" = NotEqual
binOpToMOp "<=" = LessEqual
binOpToMOp ">=" = GreaterEqual
binOpToMOp "<"  = Less
binOpToMOp ">"  = Greater
binOpToMOp "&&" = And
binOpToMOp "||" = Or
binOpToMOp op = error ("Unknown operator: " ++ op)

translate :: TypeEnv -> Expr p -> MExpr
translate _ (EVar _ x) = MVar x
translate _ (EBool _ b) = MBool  b
translate _ (EInt _ n) = MInt n
translate _ (EString _ s) = MString s
translate _ (ENil _) = MNil
translate c (EUnOp _ UnOpMinus e) = MOp Neg $ MTuple [translate c e]
translate c (EUnOp _ UnOpNot e) = MOp Not $ MTuple [translate c e]
translate c (EBinOp _ (BinOp op) e1 e2) = MOp (binOpToMOp op) (MTuple [translate c e1, translate c e2])
translate c (ELambda _ args e) = MLambda args $ translate c e
translate c (EApp _ e1 e2) = MApp (translate c e1) (translate c e2)
translate c (EIf _ e0 e1 e2) = MIf (translate c e0) (translate c e1) (translate c e2)
translate c (ELet _ x e1 e2) = MApp (MLambda [x] $ translate c e2) $ MTuple [translate c e1]
translate c (ELetTuple _ xs e1 e2) = MApp (MLambda xs $ translate c e2) $ translate c e1
translate c (EAction _ name e) =
  let Just (TypeScheme _ (TArrow _ (EffLabel eff _) _)) = Map.lookup name c in
  MAction name eff $ translate c e
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
    tp ((FunDef _ "main" [] body) : funs) acc = tp funs $ cross (const $ Just $ translate c body) id acc
    tp ((FunDef _ name args body) : funs) (_, env) =
      let env' = Map.insert name (VClosure env' args $ translate c body) env in
      tp funs (if name == "main" then Just $ translate c body else Nothing, env')
-- translateProgram [] = (Nothing, Map.empty)
-- translateProgram ((FunDef p "main" [] body) : funs) = cross (const $ Just $ translate body) id $ translateProgram funs
-- translateProgram ((FunDef p name args body) : funs) =
--   cross id (Map.insert name (MLambda args $ translate body)) $ translateProgram funs