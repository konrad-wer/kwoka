import AST
import TypeInference
import Control.Monad.State
import qualified Data.Map as Map

type Test = Bool
type TestName = String

rewriteRowTest1 :: Test
rewriteRowTest1 =
  case evalStateT (TypeInference.rewriteRow () "c"  (EffLabel "a" . EffLabel "b"  . EffLabel "c"  . EffLabel "d" $ EffEmpty)) 0 of
    Right (EffLabel "a" (EffLabel "b" (EffLabel "d" EffEmpty )), s) ->  Map.size s == 0
    _ -> False

rewriteRowTest2 :: Test
rewriteRowTest2 =
  case evalStateT (TypeInference.rewriteRow () "f"  (EffLabel "a" . EffLabel "b"  . EffLabel "c"  . EffLabel "d" $ EffVar "x")) 0 of
    Right (EffLabel "a" (EffLabel "b" (EffLabel "c" (EffLabel "d" (EffVar "#0")))), s) ->
      Map.size s == 1 && Map.lookup "x" s == Just (EffSubst (EffLabel "f" $ EffVar "#0"))
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
  case evalStateT (unifyRow () (EffLabel "a" . EffLabel "b"  . EffLabel "c"  . EffLabel "d" $ EffEmpty) (EffVar "x")) 0 of
    Right s -> Map.size s == 5 && apply s (EffVar "x") == (EffLabel "a" . EffLabel "b"  . EffLabel "c"  . EffLabel "d" $ EffEmpty)
    _ -> False

unifyRowTest6 :: Test
unifyRowTest6 =
  case evalStateT (unifyRow () (EffLabel "a" $ EffVar "x") (EffLabel "b" $ EffVar "x")) 0 of
    Left (CyclicSubstInRowError () (EffLabel "a" (EffVar "x")) (EffLabel "b" (EffVar "x"))) -> True
    _ -> False

unifyRowTest7 :: Test
unifyRowTest7 =
  case evalStateT (unifyRow () EffEmpty (EffVar "x")) 0 of
    Right s -> Map.size s == 1 && apply s (EffVar "x") == EffEmpty
    _ -> False

unifyRowTest8 :: Test
unifyRowTest8 =
  case evalStateT (unifyRow () (EffLabel "a" . EffLabel "b"  . EffLabel "c"  . EffLabel "d" $ EffVar "y")
                               (EffLabel "x" . EffLabel "y"  . EffLabel "z"  . EffLabel "k" $ EffVar "x")) 0 of
    Right s -> apply s (EffVar "x") == (EffLabel "a" . EffLabel "b"  . EffLabel "c" . EffLabel "d" $ EffVar "#3")
    _ -> False

unifyRowTest9 :: Test
unifyRowTest9 =
  case evalStateT (unifyRow () (EffVar "y") ( EffVar "x")) 0 of
    Right s -> Map.size s == 1 && apply s (EffVar "y") == EffVar "x"
    _ -> False

unifyRowTest10 :: Test
unifyRowTest10 =
  case evalStateT (unifyRow () (EffVar "x") ( EffVar "x")) 0 of
    Right s -> Map.size s == 0
    _ -> False

unifyRowTest11 :: Test
unifyRowTest11 =
  case evalStateT (unifyRow () (EffLabel "a" . EffLabel "b" $ EffVar "x") (EffLabel "a" . EffLabel "b" $ EffVar "x")) 0 of
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
  case evalStateT (unify () (TVar "x") (TVar "x")) 0 of
    Right s -> Map.size s == 0
    _ -> False

unifyTest4 :: Test
unifyTest4 =
  case evalStateT (unify () (TVar "x") (TVar "y")) 0 of
    Right s -> Map.size s == 1 && apply s (TVar "x") == TVar "y"
    _ -> False

unifyTest5 :: Test
unifyTest5 =
  case evalStateT (unify () (TProduct [TVar "x", TArrow TInt EffEmpty TInt]) (TVar "x")) 0 of
    Left (CyclicSubstInTypeError () "x" (TProduct [TVar "x", TArrow TInt EffEmpty TInt])) -> True
    _ -> False

unifyTest6 :: Test
unifyTest6 =
  case evalStateT (unify () (TProduct [TVar "y", TArrow TInt EffEmpty TInt]) (TVar "x")) 0 of
    Right s -> Map.size s == 1 && apply s (TVar "x") == TProduct [TVar "y", TArrow TInt EffEmpty TInt]
    _ -> False

unifyTest7 :: Test
unifyTest7 =
  case evalStateT (unify () (TArrow TInt  (EffLabel "a" . EffLabel "b" $ EffVar "y") TInt) (TVar "x")) 0 of
    Right s -> Map.size s == 1 && apply s (TVar "x") == TArrow TInt  (EffLabel "a" . EffLabel "b" $ EffVar "y") TInt
    _ -> False

unifyTest8 :: Test
unifyTest8 =
  case evalStateT (unify () (TArrow TInt  (EffLabel "a" . EffLabel "b" $ EffVar "x") TInt) (TVar "x")) 0 of
    Left (CyclicSubstInTypeError () "x" (TArrow TInt  (EffLabel "a" (EffLabel "b" (EffVar "x"))) TInt)) -> True
    _ -> False

unifyTest9 :: Test
unifyTest9 =
  case evalStateT (unify () (TArrow (TVar "k")  (EffLabel "a" . EffLabel "b" $ EffVar "x") TInt)
                            (TArrow TInt  (EffLabel "x" . EffLabel "z" $ EffVar "y")  (TVar "r"))) 0 of
    Right _ -> True
    _ -> False

unifyTest10 :: Test
unifyTest10 =
  case evalStateT (unify () (TArrow TInt (EffLabel "a" . EffLabel "b" $ EffVar "x") TInt)
                            (TArrow TInt (EffLabel "x" . EffLabel "z" $ EffVar "x") TInt)) 0 of
    Left (CyclicSubstInRowError () (EffLabel "a" (EffLabel "b" (EffVar "x")))
                                   (EffLabel "x" (EffLabel "z" (EffVar "x")))) -> True
    _ -> False

unifyTest11 :: Test
unifyTest11 =
  case evalStateT (unify () (TArrow (TVar "k") (EffLabel "a" . EffLabel "b" $ EffEmpty) TInt)
                            (TArrow TInt  (EffLabel "x" . EffLabel "z" $ EffEmpty) (TVar "n"))) 0 of
    Left (NoLabelInClosedRowError () "a" (EffLabel "x" (EffLabel "z" EffEmpty))) -> True
    _ -> False

unifyTest12 :: Test
unifyTest12 =
  case evalStateT (unify () (TArrow (TVar "k") (EffVar "r") (TVar "k"))
                            (TArrow TInt  (EffLabel "x" . EffLabel "z" $ EffEmpty) TInt)) 0 of
    Right s -> Map.size s == 2  && apply s (TArrow (TVar "k") (EffVar "r") (TVar "k")) ==
                                           TArrow TInt (EffLabel "x" . EffLabel "z" $ EffEmpty) TInt
    _ -> False

unifyTest13 :: Test
unifyTest13 =
  case evalStateT (unify () (TArrow TInt (EffVar "r") TInt)
                            (TArrow TInt  (EffLabel "x" . EffLabel "z" $ EffEmpty) TInt)) 0 of
    Right s -> Map.size s == 1  && apply s (TArrow TInt (EffVar "r") TInt) ==
                                           TArrow TInt (EffLabel "x" . EffLabel "z" $ EffEmpty) TInt
    _ -> False

unifyTest14 :: Test
unifyTest14 =
  case evalStateT (unify () (TArrow (TVar "k") (EffLabel "a" . EffLabel "b" $ EffEmpty) (TVar "k"))
                            (TArrow TInt  (EffLabel "x" . EffLabel "z" $ EffEmpty) TInt)) 0 of
    Left (NoLabelInClosedRowError () "a" (EffLabel "x" (EffLabel "z" EffEmpty))) -> True
    _ -> False

unifyTest15 :: Test
unifyTest15 =
  case evalStateT (unify () (TArrow TBool  (EffVar "r") (TVar "k"))
                            (TArrow TInt  (EffLabel "x" . EffLabel "z" $ EffEmpty) TInt)) 0 of
    Left (TypesMismatchError () TBool TInt) -> True
    _ -> False

unifyTest16 :: Test
unifyTest16 =
  case evalStateT (unify () (TProduct [TInt, TVar "k"]) (TProduct [TVar "k", TInt])) 0 of
    Right s -> Map.size s == 1  && apply s (TProduct [TInt, TVar "k"]) == apply s (TProduct [TVar "k", TInt]) &&
                                   apply s (TProduct [TInt, TVar "k"]) == apply s (TProduct [TInt, TInt])
    _ -> False

unifyTest17 :: Test
unifyTest17 =
  case evalStateT (unify () (TArrow (TVar "k") (EffVar "r") (TVar "k"))
                            (TArrow (TVar "k") (EffVar "r") (TVar "k"))) 0 of
    Right s -> Map.size s == 0 && apply s (TArrow (TVar "k") (EffVar "r") (TVar "k")) ==
                                         TArrow (TVar "k") (EffVar "r") (TVar "k")
    _ -> False

unifyTest18 :: Test
unifyTest18 =
  case evalStateT (unify () (TArrow (TVar "x")  (EffVar "r") TString)
                            (TArrow TInt  (EffVar "r") (TVar "y"))) 0 of
    Right s -> Map.size s == 2 && apply s (TArrow (TVar "x")  (EffVar "r") TString) ==
                                          (TArrow TInt  (EffVar "r") TString)
    _ -> False

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
  case flip evalStateT 0 $ check Map.empty Map.empty (EInt () 44) (TVar "k") $ EffLabel "Pink Floyd" EffEmpty of
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
      TInt  (EffLabel "Pink Floyd" . EffLabel "Dark Side of the Moon" . EffLabel "Time" $ EffVar "1973") of
    Right _ -> True
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
         ("checkTypeTest1", checkTypeTest1),
         ("checkTypeTest2", checkTypeTest2),
         ("checkTypeTest3", checkTypeTest3),
         ("checkTypeTest4", checkTypeTest4),
         ("checkTypeTest5", checkTypeTest5),
         ("checkTypeTest6", checkTypeTest6),
         ("checkTypeTest7", checkTypeTest7)]

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
  else
    putStrLn "One or more tests have failed! :("
  return ()
