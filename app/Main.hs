module Main where

import Parser
import AST
import System.Environment
import Text.Megaparsec.Error (errorBundlePretty)

dumpPosDef :: TopLevelDef p -> TopLevelDef ()
dumpPosDef (DefFun f) = DefFun $ dumpPosFun f
dumpPosDef (DefEff e) = DefEff $ dumpPosEff e

dumpPosEff :: EffectDef p -> EffectDef ()
dumpPosEff (EffectDef _ name as) = EffectDef () name $ map dumpPosAction as

dumpPosAction :: ActionDef p -> ActionDef ()
dumpPosAction (ActionDef _ name args t) = ActionDef () name args t

dumpPosFun :: FunDef p -> FunDef ()
dumpPosFun (FunDef _ name args effects t e) = FunDef () name args effects t $ dumpPos e

dumpPos :: Expr p -> Expr ()
dumpPos (EVar    _ x) = EVar () x
dumpPos (EUnit   _) = EUnit ()
dumpPos (EBool   _ b) = EBool () b
dumpPos (EInt    _ n) = EInt () n
dumpPos (EString _ s) = EString () s
dumpPos (ELambda _ x e) = ELambda () x $ dumpPos e
dumpPos (EApp    _  e1 e2) = EApp () (dumpPos e1) $ dumpPos e2
dumpPos (EIf     _ e1 e2 e3) = EIf () (dumpPos e1) (dumpPos e2) $ dumpPos e3
dumpPos (ELet    _ x t e1 e2) = ELet () x t (dumpPos e1) $ dumpPos e2
dumpPos (EAction _ x e) = EAction () x $ dumpPos e
dumpPos (EHandle _ e c) = EHandle () (dumpPos e) (dumpPosClause <$> c)
dumpPos (EAnnot  _ e t) = EAnnot () (dumpPos e) t
dumpPos (ETuple  _ es) = ETuple () $ dumpPos <$> es
dumpPos (EBinOp  _ op e1 e2) = EBinOp  () op (dumpPos e1) $ dumpPos e2
dumpPos (EUnOp   _ op e) = EUnOp () op $ dumpPos e

dumpPosClause :: Clause a -> Clause ()
dumpPosClause (Clause _ name args e) = Clause () name args (dumpPos e)

readArgs :: [a] -> Maybe a
readArgs [] = Nothing
readArgs xs = return $ head xs

main :: IO ()
main = do
  args <- readArgs <$> getArgs
  case args of
    Nothing -> putStrLn "Please provide filename!"
    Just filename -> do
      sourceCode <- readFile filename
      case parseProgram filename sourceCode of
        Left err -> putStrLn $ errorBundlePretty err
        Right ast -> putStrLn $ showProgram ast