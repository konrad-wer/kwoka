module Machine where

import MachineAST

eval :: MExpr -> RuntimeEnv -> Stack -> MetaStack -> IO MValue
eval (MVar x) env s ms = stack s (envLookup x env) ms
eval MNil _ s ms = stack s (VList []) ms
eval (MInt n) _ s ms = stack s (VInt n) ms
eval (MBool b) _ s ms = stack s (VBool b) ms
eval (MString str) _ s ms = stack s (VString str) ms
eval (MLambda args body) env s ms = stack s (VClosure env args body) ms
eval (MApp e1 e2) env s ms = eval e1 env (FArg env e2 : s) ms
eval (MIf e0 e1 e2) env s ms = eval e0 env (FIf env e1 e2 : s) ms
eval (MOp op e) env s ms = eval e env (FOp op : s) ms
eval (MTuple []) _ s ms = stack s (VTuple []) ms
eval (MTuple (e : es)) env s ms = eval e env (FTuple env [] es : s) ms
eval (MCase e0 e1 ptrn e2) env s ms = eval e0 env (FCase env e1 ptrn e2 : s) ms
eval (MAction name eff e) env s ms = eval e env (FAction name eff : s) ms
eval (MHandle eff e handlers) env s ms = eval e env [] (MMetaFrame (eff, handlers, env) s : ms)

stack :: Stack -> MValue -> MetaStack -> IO MValue
stack [] v ms = mstack ms v
stack (FArg env e : s) (VClosure env' args body) ms = eval e env (FClosure env' args body : s) ms
stack (FArg env e : s) (VReifiedMetaStack rms) ms = eval e env (FReifiedMetaStack rms : s) ms
stack (FOp Not : s) (VTuple [VBool b]) ms = stack s (VBool b) ms
stack (FOp Neg : s) (VTuple [VInt n]) ms = stack s (VInt n) ms
stack (FOp Mult : s) (VTuple [VInt n1, VInt n2]) ms = stack s (VInt $ n1 * n2) ms
stack (FOp Div : s) (VTuple [VInt n1, VInt n2]) ms = stack s (VInt $ n1 `div` n2) ms
stack (FOp Mod : s) (VTuple [VInt n1, VInt n2]) ms = stack s (VInt $ n1 `mod` n2) ms
stack (FOp Add : s) (VTuple [VInt n1, VInt n2]) ms = stack s (VInt $ n1 + n2) ms
stack (FOp Sub : s) (VTuple [VInt n1, VInt n2]) ms = stack s (VInt $ n1 - n2) ms
stack (FOp Concat : s) (VTuple [VString n1, VString n2]) ms = stack s (VString $ n1 ++ n2) ms
stack (FOp Cons : s) (VTuple [v, VList vs]) ms = stack s (VList $ v : vs) ms
stack (FOp Append : s) (VTuple [VList vs1, VList vs2]) ms = stack s (VList $ vs1 ++ vs2) ms
stack (FOp Equal : s) (VTuple [v1, v2]) ms = stack s (VBool (v1 == v2)) ms
stack (FOp NotEqual : s) (VTuple [v1, v2]) ms = stack s (VBool (v1 /= v2)) ms
stack (FOp LessEqual : s) (VTuple [v1, v2]) ms = stack s (VBool (v1 <= v2)) ms
stack (FOp GreaterEqual : s) (VTuple [v1, v2]) ms = stack s (VBool (v1 >= v2)) ms
stack (FOp Less : s) (VTuple [v1, v2]) ms = stack s (VBool (v1 < v2)) ms
stack (FOp Greater : s) (VTuple [v1, v2]) ms = stack s (VBool (v1 > v2)) ms
stack (FOp And : s) (VTuple [VBool b1, VBool b2]) ms = stack s (VBool $ b1 && b2) ms
stack (FOp Or : s) (VTuple [VBool b1, VBool b2]) ms = stack s (VBool $ b1 || b2) ms
stack (FOp Print : s) (VTuple [v]) ms = print v >> stack s (VTuple []) ms
stack (FOp GetLine : s) (VTuple []) ms = do
  str <- getLine
  stack s (VString str) ms
stack (FOp ReadLnInt  : s) (VTuple []) ms = do
  n <- readLn :: IO Integer
  stack s (VInt n) ms
stack (FTuple _ vs [] : s) v ms = stack s (VTuple $ reverse (v : vs)) ms
stack (FTuple env vs (e : es) : s) v ms = eval e env (FTuple env (v : vs) es : s) ms
stack (FClosure env args body : s) (VTuple vs) ms =
  let env' = foldl (flip $ uncurry envInsert) env $ zip args vs in
  eval body env' s ms
stack (FIf env e1 _ : s) (VBool True) ms = eval e1 env s ms
stack (FIf env _ e2 : s) (VBool False) ms = eval e2 env s ms
stack (FCase env e1 _ _: s) (VList []) ms = eval e1 env s ms
stack (FCase env _ (x, xs) e2 : s) (VList (v : vs)) ms =
  let env' = envInsert x v (envInsert xs (VList vs) env) in
  eval e2 env' s ms
stack (FAction name eff : s) v ms = action (name, eff) s ms v []
stack (FReifiedMetaStack rms : s) (VTuple [v]) ms = resume rms s ms v
stack _ _ _ =  error "Internal interpreter error - not exhaustive patterns in stack function"

action :: (MVar, MVar) -> Stack -> MetaStack -> MValue -> MetaStack -> IO MValue
action (name, eff) s (MMetaFrame h@(eff', handlers, env) s' : ms) (VTuple vs) rms
  | eff /= eff' = action (name, eff) s' ms (VTuple vs) (MMetaFrame h s : rms)
  | otherwise =
    let MClause args e = getHandler name handlers in
    let env' = foldl (flip $ uncurry envInsert) env $ zip args vs in
    let env'' = envInsert "resume" (VReifiedMetaStack (MMetaFrame h s : rms)) env' in
    eval e env'' s' ms
action _ _ _ _ _  = error "Internal interpreter error - not exhaustive handlers"


resume :: MetaStack -> Stack -> MetaStack -> MValue -> IO MValue
resume [] s ms v = stack s v ms
resume (MMetaFrame h s' : rms) s ms v = resume rms s' (MMetaFrame h s : ms) v

mstack :: MetaStack -> MValue -> IO MValue
mstack [] v = return v
mstack (MMetaFrame (_, handlers, env) s : ms) v =
  let MClause [x] e = getHandler "return" handlers in
  eval e (envInsert x v env) s ms