module MachineAST where

import CommonUtils
import Data.List
import qualified Data.Map as Map

type MVar = String
type RuntimeEnv = Map.Map MVar MValue

emptyEnv :: RuntimeEnv
emptyEnv = Map.empty

envLookup :: MVar -> RuntimeEnv -> MValue
envLookup = flip (Map.!)

envInsert ::  MVar -> MValue -> RuntimeEnv -> RuntimeEnv
envInsert = Map.insert

data MOp
  = Not | Neg | Mult | Div | Mod | Add | Sub | Concat | Cons | Append | Equal | NotEqual
  | LessEqual | GreaterEqual | Less | Greater | And | Or deriving (Show, Eq)

data MValue
  = VInt Integer
  | VBool Bool
  | VString String
  | VList [MValue]
  | VTuple [MValue]
  | VClosure RuntimeEnv [MVar] MExpr
  | VReifiedMetaStack MetaStack

instance Show MValue where
  show (VInt n) = show n
  show (VBool b) = show b
  show (VString s) = s
  show (VList vs) = addBrackets . intercalate ", " $ map show vs
  show (VTuple vs) = addParens . intercalate ", " $ map show vs
  show VClosure{} = "function"
  show VReifiedMetaStack{} = "resumption"

instance Eq MValue where
  (==) (VInt n1) (VInt n2) = n1 == n2
  (==) (VBool b1) (VBool b2) = b1 == b2
  (==) (VString s1) (VString s2) = s1 == s2
  (==) (VList v1) (VList v2) = v1 == v2
  (==) (VTuple t1) (VTuple t2) = t1 == t2
  (==) _ _ = False

instance Ord MValue where
  (<=) (VInt n1) (VInt n2) = n1 <= n2
  (<=) (VBool b1) (VBool b2) = b1 <= b2
  (<=) (VString s1) (VString s2) = s1 <= s2
  (<=) (VList v1) (VList v2) = v1 <= v2
  (<=) (VTuple t1) (VTuple t2) = t1 <= t2
  (<=) _ _ = False

data MExpr
  = MVar    MVar
  | MInt    Integer
  | MBool   Bool
  | MString String
  | MNil
  | MTuple  [MExpr]
  | MOp     MOp MExpr
  | MLambda [MVar] MExpr
  | MApp    MExpr MExpr
  | MIf     MExpr MExpr MExpr
  | MAction MVar MVar MExpr
  | MCase   MExpr MExpr (MVar, MVar) MExpr
  | MHandle MVar MExpr (Map.Map MVar MClause)
  deriving (Show, Eq)

data MClause = MClause [MVar] MExpr deriving (Show, Eq)

getHandler :: MVar -> Map.Map MVar MClause -> MClause
getHandler = flip (Map.!)

data MFrame
  = FArg RuntimeEnv MExpr
  | FClosure RuntimeEnv [MVar] MExpr
  | FAction MVar MVar
  | FOp MOp
  | FTuple RuntimeEnv [MValue] [MExpr]
  | FIf RuntimeEnv MExpr MExpr
  | FCase RuntimeEnv MExpr (MVar, MVar) MExpr
  | FReifiedMetaStack MetaStack
  deriving (Show, Eq)

type Stack = [MFrame]

data MMetaFrame = MMetaFrame (MVar, Map.Map MVar MClause, RuntimeEnv) Stack deriving (Show, Eq)

type MetaStack = [MMetaFrame]