import AST
import ASTBuilder
import TypeInference
import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.List as List

type Test = Bool
type TestName = String

rewriteRowTest1 :: Test
rewriteRowTest1 =
  case evalStateT (TypeInference.rewriteRow () "c"  (EffLabel "a" . EffLabel "b"  . EffLabel "c"  . EffLabel "d" $ EffEmpty)) 0 of
    Right (EffLabel "a" (EffLabel "b" (EffLabel "d" EffEmpty )), s) ->  Map.size s == 0
    _ -> False

rewriteRowTest2 :: Test
rewriteRowTest2 =
  case evalStateT (TypeInference.rewriteRow () "f"  (EffLabel "a" . EffLabel "b"  . EffLabel "c"  . EffLabel "d" $ EffVar (E "x"))) 0 of
    Right (EffLabel "a" (EffLabel "b" (EffLabel "c" (EffLabel "d" (EffVar (E "#0"))))), s) ->
      Map.size s == 1 && Map.lookup (E "x") s == Just (EffSubst (EffLabel "f" $ EffVar (E "#0")))
    _ -> False

rewriteRowTest3 :: Test
rewriteRowTest3 =
  case evalStateT (TypeInference.rewriteRow () "f"  (EffLabel "a" . EffLabel "b"  . EffLabel "c"  . EffLabel "d" $ EffEmpty)) 0 of
    Left (NoLabelInClosedRowError () "f" (EffLabel "a" (EffLabel "b" (EffLabel "c" (EffLabel "d" EffEmpty))))) -> True
    _ -> False

unifyRowTest1 :: Test
unifyRowTest1 =
  case evalStateT (unifyRow () (EffLabel "a" . EffLabel "b"  . EffLabel "c"  . EffLabel "d" $ EffEmpty)
                               (EffLabel "a" . EffLabel "b"  . EffLabel "c"  . EffLabel "d" $ EffEmpty)) 0 of
    Right s -> Map.size s == 0
    Left _ -> False

unifyRowTest2 :: Test
unifyRowTest2 =
  case evalStateT (unifyRow () (EffLabel "a" . EffLabel "b"  . EffLabel "c"  . EffLabel "d" $ EffEmpty)
                               (EffLabel "c" . EffLabel "b"  . EffLabel "d"  . EffLabel "a" $ EffEmpty)) 0 of
    Right s -> Map.size s == 0
    Left _ -> False

unifyRowTest3 :: Test
unifyRowTest3 =
  case evalStateT (unifyRow () (EffLabel "a" . EffLabel "b"  . EffLabel "c"  . EffLabel "d" $ EffEmpty)
                               EffEmpty) 0 of
    Left (RowsNotEqualError () (EffLabel "a" (EffLabel "b" (EffLabel "c" (EffLabel "d" EffEmpty)))) EffEmpty) -> True
    _ -> False

unifyRowTest4 :: Test
unifyRowTest4 =
  case evalStateT (unifyRow () (EffLabel "a" . EffLabel "b"  . EffLabel "c"  . EffLabel "d" $ EffEmpty)
                               (EffLabel "x" . EffLabel "y"  . EffLabel "z"  . EffLabel "b" $ EffEmpty)) 0 of
    Left (NoLabelInClosedRowError () "a" (EffLabel "x" (EffLabel "y" (EffLabel "z" (EffLabel "b" EffEmpty))))) -> True
    _ -> False

unifyRowTest5 :: Test
unifyRowTest5 =
  case evalStateT (unifyRow () (EffLabel "a" . EffLabel "b"  . EffLabel "c"  . EffLabel "d" $ EffEmpty) (EffVar (E "x"))) 0 of
    Right s -> Map.size s == 5 && apply s (EffVar (E "x")) == (EffLabel "a" . EffLabel "b"  . EffLabel "c"  . EffLabel "d" $ EffEmpty)
    _ -> False

unifyRowTest6 :: Test
unifyRowTest6 =
  case evalStateT (unifyRow () (EffLabel "a" $ EffVar (E "x")) (EffLabel "b" $ EffVar (E "x"))) 0 of
    Left (CyclicSubstInRowError () (EffLabel "a" (EffVar (E "x"))) (EffLabel "b" (EffVar (E "x")))) -> True
    _ -> False

unifyRowTest7 :: Test
unifyRowTest7 =
  case evalStateT (unifyRow () EffEmpty (EffVar (E "x"))) 0 of
    Right s -> Map.size s == 1 && apply s (EffVar (E "x")) == EffEmpty
    _ -> False

unifyRowTest8 :: Test
unifyRowTest8 =
  case evalStateT (unifyRow () (EffLabel "a" . EffLabel "b"  . EffLabel "c"  . EffLabel "d" $ EffVar (E "y"))
                               (EffLabel "x" . EffLabel "y"  . EffLabel "z"  . EffLabel "k" $ EffVar (E "x"))) 0 of
    Right s -> apply s (EffVar (E "x")) == (EffLabel "a" . EffLabel "b" . EffLabel "c" . EffLabel "d" $ EffVar (E "#3"))
    _ -> False

unifyRowTest9 :: Test
unifyRowTest9 =
  case evalStateT (unifyRow () (EffVar (E "y")) ( EffVar (E "x"))) 0 of
    Right s -> Map.size s == 1 && apply s (EffVar (E "y")) == EffVar (E "x")
    _ -> False

unifyRowTest10 :: Test
unifyRowTest10 =
  case evalStateT (unifyRow () (EffVar (E "x")) ( EffVar (E "x"))) 0 of
    Right s -> Map.size s == 0
    _ -> False

unifyRowTest11 :: Test
unifyRowTest11 =
  case evalStateT (unifyRow () (EffLabel "a" . EffLabel "b" $ EffVar (E "x")) (EffLabel "a" . EffLabel "b" $ EffVar (E "x"))) 0 of
    Right s -> Map.size s == 0
    _ -> False

unifyTest1 :: Test
unifyTest1 =
  case evalStateT (unify () (TProduct [TString, TBool, TInt]) (TProduct [TString, TBool, TInt])) 0 of
    Right s -> Map.size s == 0
    _ -> False

unifyTest2 :: Test
unifyTest2 =
  case evalStateT (unify () (TProduct [TString, TBool, TInt]) (TProduct [TString, TInt, TBool])) 0 of
    Left (TypesMismatchError () TBool TInt) -> True
    _ -> False

unifyTest3 :: Test
unifyTest3 =
  case evalStateT (unify () (TVar (T "x")) (TVar (T "x"))) 0 of
    Right s -> Map.size s == 0
    _ -> False

unifyTest4 :: Test
unifyTest4 =
  case evalStateT (unify () (TVar (T "x")) (TVar (T "y"))) 0 of
    Right s -> Map.size s == 1 && apply s (TVar (T "x")) == TVar (T "y")
    _ -> False

unifyTest5 :: Test
unifyTest5 =
  case evalStateT (unify () (TProduct [TVar (T "x"), TArrow TInt EffEmpty TInt]) (TVar (T "x"))) 0 of
    Left (CyclicSubstInTypeError () (T "x") (TProduct [TVar (T "x"), TArrow TInt EffEmpty TInt])) -> True
    _ -> False

unifyTest6 :: Test
unifyTest6 =
  case evalStateT (unify () (TProduct [TVar (T "y"), TArrow TInt EffEmpty TInt]) (TVar (T "x"))) 0 of
    Right s -> Map.size s == 1 && apply s (TVar (T "x")) == TProduct [TVar (T "y"), TArrow TInt EffEmpty TInt]
    _ -> False

unifyTest7 :: Test
unifyTest7 =
  case evalStateT (unify () (TArrow TInt  (EffLabel "a" . EffLabel "b" $ EffVar (E "y")) TInt) (TVar (T "x"))) 0 of
    Right s -> Map.size s == 1 && apply s (TVar (T "x")) == TArrow TInt  (EffLabel "a" . EffLabel "b" $ EffVar (E "y")) TInt
    _ -> False

unifyTest8 :: Test
unifyTest8 =
  case evalStateT (unify () (TArrow TInt (EffLabel "a" . EffLabel "b" $ EffVar (E "x")) TInt) (TVar (T "x"))) 0 of
    Right s -> Map.size s == 1 && apply s (TVar (T "x")) == TArrow TInt (EffLabel "a" . EffLabel "b" $ EffVar (E "x")) TInt
    _ -> False

unifyTest9 :: Test
unifyTest9 =
  case evalStateT (unify () (TArrow (TVar (T "k"))  (EffLabel "a" . EffLabel "b" $ EffVar (E "x")) TInt)
                            (TArrow TInt  (EffLabel "x" . EffLabel "z" $ EffVar (E "y"))  (TVar (T "r")))) 0 of
    Right _ -> True
    _ -> False

unifyTest10 :: Test
unifyTest10 =
  case evalStateT (unify () (TArrow TInt (EffLabel "a" . EffLabel "b" $ EffVar (E "x")) TInt)
                            (TArrow TInt (EffLabel "x" . EffLabel "z" $ EffVar (E "x")) TInt)) 0 of
    Left (CyclicSubstInRowError () (EffLabel "a" (EffLabel "b" (EffVar (E "x"))))
                                   (EffLabel "x" (EffLabel "z" (EffVar (E "x"))))) -> True
    _ -> False

unifyTest11 :: Test
unifyTest11 =
  case evalStateT (unify () (TArrow (TVar (T "k")) (EffLabel "a" . EffLabel "b" $ EffEmpty) TInt)
                            (TArrow TInt  (EffLabel "x" . EffLabel "z" $ EffEmpty) (TVar (T "n")))) 0 of
    Left (NoLabelInClosedRowError () "a" (EffLabel "x" (EffLabel "z" EffEmpty))) -> True
    _ -> False

unifyTest12 :: Test
unifyTest12 =
  case evalStateT (unify () (TArrow (TVar (T "k")) (EffVar (E "r")) (TVar (T "k")))
                            (TArrow TInt  (EffLabel "x" . EffLabel "z" $ EffEmpty) TInt)) 0 of
    Right s -> Map.size s == 2  && apply s (TArrow (TVar (T "k")) (EffVar (E "r")) (TVar (T "k"))) ==
                                           TArrow TInt (EffLabel "x" . EffLabel "z" $ EffEmpty) TInt
    _ -> False

unifyTest13 :: Test
unifyTest13 =
  case evalStateT (unify () (TArrow TInt (EffVar (E "r")) TInt)
                            (TArrow TInt  (EffLabel "x" . EffLabel "z" $ EffEmpty) TInt)) 0 of
    Right s -> Map.size s == 1  && apply s (TArrow TInt (EffVar (E "r")) TInt) ==
                                           TArrow TInt (EffLabel "x" . EffLabel "z" $ EffEmpty) TInt
    _ -> False

unifyTest14 :: Test
unifyTest14 =
  case evalStateT (unify () (TArrow (TVar (T "k")) (EffLabel "a" . EffLabel "b" $ EffEmpty) (TVar (T "k")))
                            (TArrow TInt  (EffLabel "x" . EffLabel "z" $ EffEmpty) TInt)) 0 of
    Left (NoLabelInClosedRowError () "a" (EffLabel "x" (EffLabel "z" EffEmpty))) -> True
    _ -> False

unifyTest15 :: Test
unifyTest15 =
  case evalStateT (unify () (TArrow TBool  (EffVar (E "r")) (TVar (T "k")))
                            (TArrow TInt  (EffLabel "x" . EffLabel "z" $ EffEmpty) TInt)) 0 of
    Left (TypesMismatchError () TBool TInt) -> True
    _ -> False

unifyTest16 :: Test
unifyTest16 =
  case evalStateT (unify () (TProduct [TInt, TVar (T "k")]) (TProduct [TVar (T "k"), TInt])) 0 of
    Right s -> Map.size s == 1  && apply s (TProduct [TInt, TVar (T "k")]) == apply s (TProduct [TVar (T "k"), TInt]) &&
                                   apply s (TProduct [TInt, TVar (T "k")]) == apply s (TProduct [TInt, TInt])
    _ -> False

unifyTest17 :: Test
unifyTest17 =
  case evalStateT (unify () (TArrow (TVar (T "k")) (EffVar (E "r")) (TVar (T "k")))
                            (TArrow (TVar (T "k")) (EffVar (E "r")) (TVar (T "k")))) 0 of
    Right s -> Map.size s == 0 && apply s (TArrow (TVar (T "k")) (EffVar (E "r")) (TVar (T "k"))) ==
                                          TArrow (TVar (T "k")) (EffVar (E "r")) (TVar (T "k"))
    _ -> False

unifyTest18 :: Test
unifyTest18 =
  case evalStateT (unify () (TArrow (TVar (T "x"))  (EffVar (E "r")) TString)
                            (TArrow TInt  (EffVar (E "r")) (TVar (T "y")))) 0 of
    Right s -> Map.size s == 2 && apply s (TArrow (TVar (T "x"))  (EffVar (E "r")) TString) ==
                                          TArrow TInt  (EffVar (E "r")) TString
    _ -> False


unifyTest19 :: Test
unifyTest19 =
  case evalStateT (unify () (TProduct []) (TProduct [TInt])) 0 of
    Left (ProductArityMismatchError () (TProduct []) (TProduct [TInt])) -> True
    _ -> False

unifyTest20 :: Test
unifyTest20 =
  case evalStateT (unify () (TProduct [TInt, TBool, TArrow TInt EffEmpty TInt, TString, TProduct []])
                            (TProduct [TInt, TBool, TArrow TInt EffEmpty TInt])) 0 of
    Left (ProductArityMismatchError () (TProduct [TInt, TBool, TArrow TInt EffEmpty TInt, TString, TProduct []])
                                       (TProduct [TInt, TBool, TArrow TInt EffEmpty TInt])) -> True
    _ -> False

unifyTest21 :: Test
unifyTest21 =
  case evalStateT (unify () (TProduct [TProduct [TProduct [TProduct [TInt]]]]) (TProduct [TProduct [TProduct []]])) 0 of
    Left (ProductArityMismatchError () (TProduct [TProduct [TInt]]) (TProduct [])) -> True
    _ -> False

unifyTest22 :: Test
unifyTest22 =
  case evalStateT (unify () (TProduct [TInt, TProduct [TInt], TArrow TInt EffEmpty TInt])
                            (TProduct [TInt, TProduct [TInt, TProduct []], TArrow TInt EffEmpty TInt])) 0 of
    Left (ProductArityMismatchError () (TProduct [TInt]) (TProduct [TInt, TProduct []])) -> True
    _ -> False

effEnv :: EffectEnv ()
effEnv = Map.fromList
  [("Exc", [ActionDef () "Raise" [TString] Nothing, ActionDef () "RaiseRet" [TString, TInt] Nothing]),
   ("IO", [ActionDef () "Read" [] $ Just TInt]),
   ("Bar", [ActionDef () "Foo" [TInt] $ Just TBool])]

actionsTypes :: TypeEnv
actionsTypes = Map.fromList
  [("Raise", TypeScheme [T "$0"] (TArrow (TProduct [TString]) (EffLabel "Exc" EffEmpty) $ TVar $ T "$0")),
   ("Read", TypeScheme [] (TArrow (TProduct []) (EffLabel "IO" EffEmpty) TInt )),
   ("RaiseRet", TypeScheme [T "$1"] (TArrow (TProduct [TString, TInt]) (EffLabel "Exc" EffEmpty) $ TVar $ T "$1")),
   ("Foo", TypeScheme [] (TArrow (TProduct [TInt]) (EffLabel "Bar" EffEmpty) TBool))]

checkTypeTest1 :: Test
checkTypeTest1 =
  case flip evalStateT 0 $ check Map.empty Map.empty (EInt () 44) TInt EffEmpty of
    Right s -> Map.size s == 1
    _ -> False

checkTypeTest2 :: Test
checkTypeTest2 =
  case flip evalStateT 0 $ check Map.empty Map.empty (EInt () 44) TInt $ EffLabel "Pink Floyd" EffEmpty of
    Right s -> Map.size s == 2
    _ -> False

checkTypeTest3 :: Test
checkTypeTest3 =
  case flip evalStateT 0 $ check Map.empty Map.empty (EInt () 44) TBool EffEmpty of
    Left (TypesMismatchError () TBool TInt) -> True
    _ -> False

checkTypeTest4 :: Test
checkTypeTest4 =
  case flip evalStateT 0 $ check Map.empty Map.empty (EInt () 44) (TVar (T "k")) $ EffLabel "Pink Floyd" EffEmpty of
    Right s -> Map.size s == 3
    _ -> False

checkTypeTest5 :: Test
checkTypeTest5 =
  case flip evalStateT 0 $ check Map.empty Map.empty
      (EApp () (ELambda () ["x"] (EUnOp () UnOpNot (EVar () "x"))) (ETuple() [EBool () True]))
      TBool EffEmpty of
    Right _ -> True
    _ -> False

checkTypeTest6 :: Test
checkTypeTest6 =
  case flip evalStateT 0 $ check Map.empty Map.empty
      (EApp () (ELambda () ["x"] (EUnOp () UnOpMinus (EVar () "x"))) (ETuple() [EInt () 44]))
      TBool EffEmpty of
    Left (TypesMismatchError () TBool TInt) -> True
    _ -> False

checkTypeTest7 :: Test
checkTypeTest7 =
  case flip evalStateT 0 $ check Map.empty Map.empty
      (EApp () (ELambda () ["x"] (EUnOp () UnOpMinus (EVar () "x"))) (ETuple() [EInt () 44]))
      TInt  (EffLabel "Pink Floyd" . EffLabel "Dark Side of the Moon" . EffLabel "Time" $ EffVar (E "1973")) of
    Right _ -> True
    _ -> False

checkTypeTest8 :: Test
checkTypeTest8 =
  case flip evalStateT 0 $ check Map.empty Map.empty
      (EApp () (ELambda () ["x"] (EUnOp () UnOpMinus (EVar () "x"))) (ETuple() [EBool () True]))
      TInt  (EffLabel "Pink Floyd" . EffLabel "Dark Side of the Moon" . EffLabel "Time" $ EffVar (E "1973")) of
    Left (TypesMismatchError () TInt TBool) -> True
    _ -> False

checkTypeTest9 :: Test
checkTypeTest9 =
  case flip evalStateT 0 $ check Map.empty actionsTypes
      (EAction () "Read" (ETuple() [EBool () True])) TInt (EffLabel "IO" EffEmpty) of
    Left (ProductArityMismatchError () (TProduct []) (TProduct [TBool])) -> True
    _ -> False

checkTypeTest10 :: Test
checkTypeTest10 =
  case flip evalStateT 0 $ check Map.empty actionsTypes
      (EAction () "Read" (ETuple() [])) TInt (EffLabel "IO" EffEmpty) of
    Right _ -> True
    _ -> False

checkTypeTest11 :: Test
checkTypeTest11 =
  case flip evalStateT 0 $ check Map.empty actionsTypes
      (EAction () "Read" (ETuple() [])) TInt (EffLabel "Exc" EffEmpty) of
    Left (RowsNotEqualError () EffEmpty (EffLabel "IO" (EffVar _))) -> True
    _ -> False

checkTypeTest12 :: Test
checkTypeTest12 =
  case flip evalStateT 0 $ check Map.empty actionsTypes
       (EAction () "RaiseRet" (ETuple() [EString () "Bestrafer ", EAction () "Read" (ETuple() [])]))
       TInt (EffLabel "Exc" (EffLabel "IO" EffEmpty)) of
    Right _ -> True
    _ -> False

checkTypeTest13 :: Test
checkTypeTest13 =
  case flip evalStateT 0 $ check Map.empty actionsTypes
       (EAction () "RaiseRet" (ETuple() [EString () "Bestrafer ", EAction () "Read" (ETuple() [])]))
       TInt (EffVar $ E "r") of
    Right _ -> True
    _ -> False

checkTypeTest14 :: Test
checkTypeTest14 =
  case flip evalStateT 0 $ check Map.empty actionsTypes
       (EAction () "RaiseRet" (ETuple() [EString () "Bestrafer ", EAction () "Read" (ETuple() [])])) TInt EffEmpty of
    Left (RowsNotEqualError () EffEmpty (EffLabel "Exc" (EffLabel "IO" (EffVar _)))) -> True
    _ -> False

checkTypeTest15 :: Test
checkTypeTest15 =
  case flip evalStateT 0 $ check Map.empty actionsTypes
       (EAction () "RaiseRet" (ETuple() [EString () "Bestrafer ", EAction () "Read" (ETuple() [])]))
       TInt (EffLabel "Exc" EffEmpty) of
    Left (RowsNotEqualError () EffEmpty (EffLabel "IO" (EffVar _))) -> True
    _ -> False

checkTypeTest16 :: Test
checkTypeTest16 =
  case flip evalStateT 0 $ check Map.empty actionsTypes
       (EAction () "RaiseRet" (ETuple() [EString () "Bestrafer ", EAction () "Read" (ETuple() [])]))
       TInt (EffLabel "Exc" EffEmpty) of
    Left (RowsNotEqualError () EffEmpty (EffLabel "IO" (EffVar _))) -> True
    _ -> False

checkTypeTest17 :: Test
checkTypeTest17 =
  case flip evalStateT 0 $ check Map.empty actionsTypes
       (EAction () "RaiseRet" (ETuple() [EAction () "Read" (ETuple() []), EAction () "Read" (ETuple() [])]))
       TInt (EffLabel "IO" (EffLabel "Exc" EffEmpty))  of
    Left (TypesMismatchError () TString TInt) -> True
    _ -> False

checkTypeTest18 :: Test
checkTypeTest18 =
  case flip evalStateT 0 $ check Map.empty actionsTypes
       (EAction () "RaiseRet" (ETuple() [EString () "44", EBool () True]))
       TInt (EffLabel "IO" (EffLabel "Exc" EffEmpty))  of
    Left (TypesMismatchError () TInt TBool) -> True
    _ -> False

checkTypeTest19 :: Test
checkTypeTest19 =
  case flip evalStateT 0 $ check Map.empty actionsTypes
       (EAction () "RaiseRet" (ETuple() [EInt () 44]))
       TInt (EffLabel "IO" (EffLabel "Exc" EffEmpty))  of
    Left (ProductArityMismatchError () (TProduct [TString, TInt]) (TProduct [TInt])) -> True
    _ -> False

checkTypeTest20 :: Test
checkTypeTest20 =
  case flip evalStateT 0 $ check Map.empty actionsTypes
       (EAction () "RaiseRet" (ETuple() [EInt () 44, EBool () True, EString () "Bestrafer"]))
       TInt (EffLabel "IO" (EffLabel "Exc" EffEmpty))  of
    Left (ProductArityMismatchError () (TProduct [TString, TInt]) (TProduct [TInt, TBool, TString])) -> True
    _ -> False

checkTypeTest21 :: Test
checkTypeTest21 =
  case flip evalStateT 0 $ check Map.empty Map.empty
      (EIf () (EBool () True) (EInt () 42) (EInt () 44)) TInt EffEmpty of
    Right _ -> True
    _ -> False

checkTypeTest22 :: Test
checkTypeTest22 =
  case flip evalStateT 0 $ check Map.empty Map.empty
      (EIf () (EBool () True) (EInt () 42) (EBool () True)) TInt EffEmpty of
    Left (TypesMismatchError () TInt TBool) -> True
    _ -> False

checkTypeTest23 :: Test
checkTypeTest23 =
  case flip evalStateT 0 $ check Map.empty Map.empty
      (EIf () (EInt () 59) (EInt () 42) (EInt () 44)) TInt EffEmpty of
    Left (TypesMismatchError () TBool TInt) -> True
    _ -> False

checkTypeTest24 :: Test
checkTypeTest24 =
  case flip evalStateT 0 $ check Map.empty actionsTypes
      (EIf () (EBool () True) (EAction () "Raise" (ETuple() [EString () "42"]))
                              (EAction () "Read" (ETuple() []))) TInt
                              (EffLabel "IO" (EffLabel "Exc" EffEmpty)) of
    Right _ -> True
    _ -> False

checkTypeTest25 :: Test
checkTypeTest25 =
  case flip evalStateT 0 $ check Map.empty actionsTypes
      (EIf () (EAction () "Foo" (ETuple() [EInt () 42]))
      (EAction () "Raise" (ETuple() [EString () "42"]))
      (EAction () "Read" (ETuple() []))) TInt
      (EffLabel "IO" (EffLabel "Exc" EffEmpty)) of
    Left (RowsNotEqualError () EffEmpty (EffLabel "Bar" (EffVar _))) -> True
    _ -> False

checkTypeTest26 :: Test
checkTypeTest26 =
  case flip evalStateT 0 $ check Map.empty actionsTypes
      (EIf () (EAction () "Foo" (ETuple() [EInt () 42]))
      (EAction () "Raise" (ETuple() [EString () "42"]))
      (EAction () "Read" (ETuple() []))) TInt
      (EffLabel "IO" (EffLabel "Exc" (EffLabel "Bar" EffEmpty))) of
    Right _ -> True
    _ -> False

checkTypeTest27 :: Test
checkTypeTest27 =
  case flip evalStateT 0 $ check Map.empty binOpTypes
      (EBinOp () (BinOp "+") (EInt () 44) (EInt () 42)) TInt EffEmpty of
    Right _ -> True
    _ -> False

checkTypeTest28 :: Test
checkTypeTest28 =
  case flip evalStateT 0 $ check Map.empty binOpTypes
                    (EBinOp () (BinOp "+") (EBinOp () (BinOp "*") (EInt () 44) (EInt () 42)) (EInt () 42))
                    TInt (EffLabel "IO" EffEmpty) of
    Right _ -> True
    _ -> False

checkTypeTest29 :: Test
checkTypeTest29 =
  case flip evalStateT 0 $ check Map.empty binOpTypes
                    (EBinOp () (BinOp "!=") (EBinOp () (BinOp "==") (EInt () 44) (EInt () 42)) (EBool () True))
                    TBool (EffLabel "IO" EffEmpty) of
    Right _ -> True
    _ -> False

checkTypeTest30 :: Test
checkTypeTest30 =
  case flip evalStateT 0 $ check Map.empty binOpTypes
                    (EBinOp () (BinOp "!=") (EBinOp () (BinOp "==") (EInt () 44) (EInt () 42)) (EInt () 42))
                    TBool (EffLabel "IO" EffEmpty) of
    Left (TypesMismatchError () TBool TInt) -> True
    _ -> False

checkTypeTest31 :: Test
checkTypeTest31 =
  case flip evalStateT 0 $ check Map.empty binOpTypes
                    (EBinOp () (BinOp "!=") (EBinOp () (BinOp "==") (EString () "Name") (EInt () 44)) (EBool () True))
                    TBool (EffLabel "IO" EffEmpty) of
    Left (TypesMismatchError () TString TInt) -> True
    _ -> False

checkTypeTest32 :: Test
checkTypeTest32 =
  case flip evalStateT 0 $ check Map.empty binOpTypes
       (ELet () "f" (ELambda () ["x"] (EVar () "x"))
       (EBinOp () (BinOp "!=") (EBinOp () (BinOp "==") (EApp () (EVar () "f") (ETuple () [EInt () 44])) (EInt () 42))
       (EApp () (EVar () "f") (ETuple () [EBool () True])))) TBool EffEmpty of
    Right _ -> True
    _ -> False

checkTypeTest33 :: Test
checkTypeTest33 =
  case flip evalStateT 0 $ check Map.empty actionsTypes
       (ELet () "f" (ELambda () ["x"]  (EAction () "Raise" (ETuple() [EVar () "x"])))
       (EAction () "Read" (ETuple() []))) TInt (EffLabel "IO" EffEmpty) of
    Right _ -> True
    _ -> False


checkTypeTest34 :: Test
checkTypeTest34 =
  case flip evalStateT 0 $ check Map.empty actionsTypes
       (ELet () "f" (EAction () "Raise" (ETuple() [EString () "42"]))
       (EAction () "Read" (ETuple() []))) TInt (EffLabel "IO" EffEmpty) of
    Left (RowsNotEqualError () EffEmpty (EffLabel "Exc" (EffVar _))) -> True
    _ -> False

checkTypeTest35 :: Test
checkTypeTest35 =
  case flip evalStateT 0 $ check Map.empty actionsTypes
       (ELet () "f" (EAction () "Raise" (ETuple() [EString () "42"]))
       (EAction () "Read" (ETuple() []))) TInt (EffLabel "IO" (EffLabel "Exc" $ EffVar $ E "r")) of
    Right _ -> True
    _ -> False

checkTypeTest36 :: Test
checkTypeTest36 =
  case flip evalStateT 0 $ check effEnv actionsTypes
       (EHandle () "Exc" (EAction () "Raise" (ETuple() [EString () "42"]))
       [Clause () "return" ["x"] (EInt () 5)]) TInt EffEmpty of
    Left (HandleActionsMismatchError () l1 l2) -> List.sort l1 == ["Raise", "RaiseRet", "return"] &&
                                                  List.sort l2 == ["return"]
    _ -> False

checkTypeTest37 :: Test
checkTypeTest37 =
  case flip evalStateT 0 $ check effEnv actionsTypes
       (EHandle () "Exc" (EAction () "Raise" (ETuple() [EString () "42"]))
       [Clause () "return" ["x"] (EInt () 5), Clause () "Raise" ["x"] (EInt () 44)]) TInt EffEmpty of
    Left (HandleActionsMismatchError () l1 l2) -> List.sort l1 == ["Raise", "RaiseRet", "return"] &&
                                                  List.sort l2 == ["Raise", "return"]
    _ -> False

checkTypeTest38 :: Test
checkTypeTest38 =
  case flip evalStateT 0 $ check effEnv actionsTypes
       (EHandle () "Exc" (EAction () "Raise" (ETuple() [EString () "42"]))
       [Clause () "return" ["x"] (EInt () 5), Clause () "Raise" ["x"] (EInt () 44),
        Clause () "RaiseRet" ["x", "y"] (EInt () 44)]) TInt EffEmpty of
    Right _ -> True
    _ -> False

checkTypeTest39 :: Test
checkTypeTest39 =
  case flip evalStateT 0 $ check effEnv actionsTypes
       (EHandle () "Exc" (EAction () "Raise" (ETuple() [EString () "42"]))
       [Clause () "return" ["x"] (EVar () "x"), Clause () "Raise" ["x"] (EInt () 44),
        Clause () "RaiseRet" ["x", "y"] (EInt () 44)]) TInt EffEmpty of
    Right _ -> True
    _ -> False

checkTypeTest40 :: Test
checkTypeTest40 =
  case flip evalStateT 0 $ check effEnv actionsTypes
       (EHandle () "Exc" (EAction () "Raise" (ETuple() [EString () "42"]))
       [Clause () "return" ["x"] (EVar () "x"), Clause () "Raise" ["x"] (EInt () 44),
        Clause () "RaiseRet" ["x", "y"] (EString () "44")]) TInt EffEmpty of
    Left (TypesMismatchError () TInt TString) -> True
    _ -> False

checkTypeTest41 :: Test
checkTypeTest41 =
  case flip evalStateT 0 $ check effEnv actionsTypes
       (EHandle () "IO" (EAction () "Read" (ETuple() []))
       [Clause () "return" ["x"] (EVar () "x"), Clause () "Read" [] (EInt () 44)]) TInt EffEmpty of
    Right _ -> True
    _ -> False

checkTypeTest42 :: Test
checkTypeTest42 =
  case flip evalStateT 0 $ check effEnv actionsTypes
       (EHandle () "IO" (EAction () "Read" (ETuple() []))
       [Clause () "return" ["x"] (EVar () "x"), Clause () "Read" [] (EString () "44")]) TInt EffEmpty of
    Left (TypesMismatchError () TInt TString) -> True
    _ -> False

checkTypeTest43 :: Test
checkTypeTest43 =
  case flip evalStateT 0 $ check effEnv actionsTypes
       (EHandle () "IO" (ELet () "x" (EAction () "Read" (ETuple() [])) (EString () "15"))
       [Clause () "return" ["x"] (EVar () "x"), Clause () "Read" [] (EString () "44")]) TString EffEmpty of
    Right _ -> True
    _ -> False

checkTypeTest44 :: Test
checkTypeTest44 =
  case flip evalStateT 0 $ check effEnv actionsTypes
       (EHandle () "IO" (ELet () "x" (EAction () "Read" (ETuple() [])) (EString () "15"))
       [Clause () "return" ["x"] (EVar () "x"), Clause () "Raise" ["x"] (EInt () 44),
        Clause () "RaiseRet" ["x", "y"] (EInt () 44)]) TString EffEmpty of
    Left (HandleActionsMismatchError () l1 l2) -> List.sort l1 == ["Read", "return"] &&
                                                  List.sort l2 == ["Raise", "RaiseRet", "return"]
    _ -> False

checkTypeTest45 :: Test
checkTypeTest45 =
  case flip evalStateT 0 $ check effEnv (Map.union binOpTypes actionsTypes)
       (EHandle () "IO" (ELet () "x" (EAction () "Read" (ETuple() [])) (EString () "15"))
       [Clause () "return" ["x"] (EBinOp () (BinOp "+") (EInt () 42) (EVar () "x")),
        Clause () "Read" [] (EString () "44")]) TString EffEmpty of
    Left _ -> True
    _ -> False

checkTypeTest46 :: Test
checkTypeTest46 =
  case flip evalStateT 0 $ check effEnv actionsTypes
       (EHandle () "IO" (ELet () "x" (EAction () "Read" (ETuple() [])) (EString () "15"))
       [Clause () "return" ["x"] (EVar () "x"),
        Clause () "Read" [] (EApp () (EVar () "resume") (ETuple() [EInt () 44]))]) TString EffEmpty of
    Right _ -> True
    _ -> False

checkTypeTest47 :: Test
checkTypeTest47 =
  case flip evalStateT 0 $ check effEnv actionsTypes
       (EHandle () "IO" (ELet () "x" (EAction () "Read" (ETuple() [])) (EString () "15"))
       [Clause () "return" ["x"] (EVar () "x"),
        Clause () "Read" [] (EApp () (EVar () "resume") (ETuple() [EBool () True]))]) TString EffEmpty of
    Left (TypesMismatchError () TInt TBool) -> True
    _ -> False

checkTypeTest48 :: Test
checkTypeTest48 =
  case flip evalStateT 0 $ check effEnv actionsTypes
       (EHandle () "IO" (ELet () "x" (EAction () "Read" (ETuple() [])) (EString () "15"))
       [Clause () "return" ["x"] (EVar () "x"),
        Clause () "Read" [] (EUnOp () UnOpNot (EApp () (EVar () "resume") (ETuple() [EInt () 44])))]) TString EffEmpty of
    Left (TypesMismatchError () TString TBool) -> True
    _ -> False

checkTypeTest49 :: Test
checkTypeTest49 =
  case flip evalStateT 0 $ check effEnv (Map.union binOpTypes actionsTypes)
       (EHandle () "Bar" (ELet () "x" (EAction () "Foo" (ETuple() [EInt () 44]))
       (EBinOp () (BinOp "&&") (EVar () "x") (EBool () True)))
       [Clause () "return" ["x"] (EVar () "x"),
        Clause () "Foo" ["x"] (EApp () (EVar () "resume")(ETuple() [EBool () False]))]) TBool EffEmpty of
    Right _ -> True
    _ -> False

checkTypeTest50 :: Test
checkTypeTest50 =
  case flip evalStateT 0 $ check effEnv (Map.union binOpTypes actionsTypes)
       (EHandle () "Bar" (ELet () "x" (EAction () "Foo" (ETuple() [EInt () 44]))
       (EBinOp () (BinOp "&&") (EVar () "x") (EBool () True)))
       [Clause () "return" ["x"] (EAction () "Read" (ETuple() [])),
        Clause () "Foo" ["x"] (EAction () "Raise" (ETuple() [EString () "44"]))]) TInt EffEmpty of
    Left (RowsNotEqualError () EffEmpty (EffLabel _ (EffLabel _ (EffVar _)))) -> True
    _ -> False

tests :: [(TestName, Test)]
tests = [("rewriteRowTest1", rewriteRowTest1),
         ("rewriteRowTest2", rewriteRowTest2),
         ("rewriteRowTest3", rewriteRowTest3),
         ("unifyRowTest1", unifyRowTest1),
         ("unifyRowTest2", unifyRowTest2),
         ("unifyRowTest3", unifyRowTest3),
         ("unifyRowTest4", unifyRowTest4),
         ("unifyRowTest5", unifyRowTest5),
         ("unifyRowTest6", unifyRowTest6),
         ("unifyRowTest7", unifyRowTest7),
         ("unifyRowTest8", unifyRowTest8),
         ("unifyRowTest9", unifyRowTest9),
         ("unifyRowTest10", unifyRowTest10),
         ("unifyRowTest11", unifyRowTest11),
         ("unifyTest1", unifyTest1),
         ("unifyTest2", unifyTest2),
         ("unifyTest3", unifyTest3),
         ("unifyTest4", unifyTest4),
         ("unifyTest5", unifyTest5),
         ("unifyTest6", unifyTest6),
         ("unifyTest7", unifyTest7),
         ("unifyTest8", unifyTest8),
         ("unifyTest9", unifyTest9),
         ("unifyTest10", unifyTest10),
         ("unifyTest11", unifyTest11),
         ("unifyTest12", unifyTest12),
         ("unifyTest13", unifyTest13),
         ("unifyTest14", unifyTest14),
         ("unifyTest15", unifyTest15),
         ("unifyTest16", unifyTest16),
         ("unifyTest17", unifyTest17),
         ("unifyTest18", unifyTest18),
         ("unifyTest19", unifyTest19),
         ("unifyTest20", unifyTest20),
         ("unifyTest21", unifyTest21),
         ("unifyTest22", unifyTest22),
         ("checkTypeTest1", checkTypeTest1),
         ("checkTypeTest2", checkTypeTest2),
         ("checkTypeTest3", checkTypeTest3),
         ("checkTypeTest4", checkTypeTest4),
         ("checkTypeTest5", checkTypeTest5),
         ("checkTypeTest6", checkTypeTest6),
         ("checkTypeTest7", checkTypeTest7),
         ("checkTypeTest8", checkTypeTest8),
         ("checkTypeTest9", checkTypeTest9),
         ("checkTypeTest10", checkTypeTest10),
         ("checkTypeTest11", checkTypeTest11),
         ("checkTypeTest12", checkTypeTest12),
         ("checkTypeTest13", checkTypeTest13),
         ("checkTypeTest14", checkTypeTest14),
         ("checkTypeTest15", checkTypeTest15),
         ("checkTypeTest16", checkTypeTest16),
         ("checkTypeTest17", checkTypeTest17),
         ("checkTypeTest18", checkTypeTest18),
         ("checkTypeTest19", checkTypeTest19),
         ("checkTypeTest20", checkTypeTest20),
         ("checkTypeTest21", checkTypeTest21),
         ("checkTypeTest22", checkTypeTest22),
         ("checkTypeTest23", checkTypeTest23),
         ("checkTypeTest24", checkTypeTest24),
         ("checkTypeTest25", checkTypeTest25),
         ("checkTypeTest26", checkTypeTest26),
         ("checkTypeTest27", checkTypeTest27),
         ("checkTypeTest28", checkTypeTest28),
         ("checkTypeTest29", checkTypeTest29),
         ("checkTypeTest30", checkTypeTest30),
         ("checkTypeTest31", checkTypeTest31),
         ("checkTypeTest32", checkTypeTest32),
         ("checkTypeTest33", checkTypeTest33),
         ("checkTypeTest34", checkTypeTest34),
         ("checkTypeTest35", checkTypeTest35),
         ("checkTypeTest36", checkTypeTest36),
         ("checkTypeTest37", checkTypeTest37),
         ("checkTypeTest38", checkTypeTest38),
         ("checkTypeTest39", checkTypeTest39),
         ("checkTypeTest40", checkTypeTest40),
         ("checkTypeTest41", checkTypeTest41),
         ("checkTypeTest42", checkTypeTest42),
         ("checkTypeTest43", checkTypeTest43),
         ("checkTypeTest44", checkTypeTest44),
         ("checkTypeTest45", checkTypeTest45),
         ("checkTypeTest46", checkTypeTest46),
         ("checkTypeTest47", checkTypeTest47),
         ("checkTypeTest48", checkTypeTest48),
         ("checkTypeTest49", checkTypeTest49),
         ("checkTypeTest50", checkTypeTest50)]

runTest :: (TestName, Test) -> String
runTest (name, t) =
  name ++ " - " ++  if t then
    "Passed\n"
  else
    "Failed!\n"

runTests :: [(TestName, Test)] -> String
runTests = foldl (flip $ flip (++) . runTest) ""

checkAll :: [(TestName, Test)] -> Bool
checkAll = foldl (flip $ (&&) . snd) True

main :: IO ()
main = do
  putStrLn ""
  putStrLn $ runTests tests
  if checkAll tests then
    putStrLn "All tests have passed :)"
  else do
    putStrLn "One or more tests have failed! :("
    fail "Test failure"
  return ()
