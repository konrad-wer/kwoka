module Parser (parseProgram) where

-- "You've never heard of the Millennium Falcon?
-- …It's the ship that made the Kessel Run in less than 0.000012 megaParsecs."

import Data.Void
import Control.Monad
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad.Combinators.Expr
import AST

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt  = L.skipLineComment "//"
    blockCmnt = L.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

symbolWithPos :: String -> Parser (SourcePos, String)
symbolWithPos x = do
  pos <- getSourcePos
  sym <- symbol x
  return (pos, sym)

comma :: Parser String
comma = symbol ","

unsignedInteger :: Parser Integer
unsignedInteger = lexeme L.decimal

stringLiteral :: Parser String
stringLiteral = lexeme $ char '\"' *> manyTill L.charLiteral (char '\"')

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

angles :: Parser a -> Parser a
angles = between (symbol "<") (symbol ">")

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

rword :: String -> Parser ()
rword w = (lexeme . try) (string w *> notFollowedBy alphaNumChar)

rws :: [String]
rws = ["let", "def", "where", "if", "then", "else", "in", "handle", "return", "of",
       "case", "False", "True", "Bool", "Int", "String", "λ", "fn", "effect"]

identifier :: Parser Var
identifier = (lexeme . try) (p >>= check)
  where
    p = (:) <$> lowerChar <*> many (alphaNumChar <|> char '_')
    check x
      | x `elem` rws = fail $ "keyword " ++ show x ++ " cannot be an identifier"
      | otherwise = return x

upperIdentifier :: Parser String
upperIdentifier = (lexeme . try) (p >>= check)
  where
    p = (:) <$> upperChar <*> many alphaNumChar
    check x
      | x `elem` rws = fail $ "keyword " ++ show x ++ " cannot be an identifier"
      | otherwise = return x

--Kwoka syntax----------------------------------------------------------

parseProgram :: String -> String -> Either (ParseErrorBundle String Void) [TopLevelDef SourcePos]
parseProgram = parse (between sc eof program)

program :: Parser [TopLevelDef SourcePos]
program = many topLevelDef

topLevelDef :: Parser (TopLevelDef SourcePos)
topLevelDef = DefFun <$> fun <|> DefEff <$> effectDef

fun :: Parser (FunDef SourcePos)
fun = do
  pos <- getSourcePos
  rword "fn"
  name <- identifier
  args <- parens $ sepBy identifier comma
  body <- braces expr
  return $ FunDef pos name args body

binOpParser :: String -> Parser (Expr SourcePos -> Expr SourcePos -> Expr SourcePos)
binOpParser x = do
  pos <- getSourcePos
  op <- symbol x
  return $ EBinOp pos (BinOp op)

operators :: [[Operator Parser (Expr SourcePos)]]
operators =
  [[Prefix (flip EUnOp UnOpMinus . fst <$> symbolWithPos "-"),
    Prefix (flip EUnOp UnOpNot . fst <$> symbolWithPos "!") ],
   [InfixL (binOpParser "*"),
    InfixL (binOpParser "/"),
    InfixL (binOpParser "%")],
   [InfixL (binOpParser "+"),
    InfixL (binOpParser "-")],
   [InfixR (binOpParser ":"),
    InfixR (binOpParser "^")],
   [InfixN (binOpParser "=="),
    InfixN (binOpParser "!="),
    InfixN (binOpParser "<="),
    InfixN (binOpParser ">="),
    InfixN (binOpParser "<"),
    InfixN (binOpParser ">")],
   [InfixL (binOpParser "&&")],
   [InfixL (binOpParser "||")]]

expr :: Parser (Expr SourcePos)
expr = makeExprParser eTerm operators

eTerm :: Parser (Expr SourcePos)
eTerm =
  try eApp <|>
  try eAction <|>
  eIf <|>
  eCase <|>
  try eLet <|>
  eLetTuple <|>
  eLambda <|>
  eSimple

eSimple :: Parser (Expr SourcePos)
eSimple =
  ETuple  <$> getSourcePos <*> pure [] <*  symbol "()" <|>
  EBool   <$> getSourcePos <*> (rword "True" >> return True) <|>
  EBool   <$> getSourcePos <*> (rword "False" >> return False) <|>
  EString <$> getSourcePos <*> stringLiteral <|>
  EInt    <$> getSourcePos <*> try (fromIntegral <$> unsignedInteger) <|>
  EVar    <$> getSourcePos <*> identifier <|>
  eList <|>
  eHandle <|>
  try (parens expr) <|>
  eTuple

eLambda :: Parser (Expr SourcePos)
eLambda = do
  pos <- getSourcePos
  void (symbol "\\" <|> symbol "λ")
  args <- sepBy identifier comma
  void $ symbol "=>"
  ELambda pos args <$> expr

eIf :: Parser (Expr SourcePos)
eIf = do
  pos <- getSourcePos
  rword "if"
  e1 <- expr
  rword "then"
  e2 <- expr
  rword "else"
  EIf pos e1 e2 <$> expr

eCase :: Parser (Expr SourcePos)
eCase = do
  pos <- getSourcePos
  rword "case"
  e1 <- expr
  rword "of"
  void $ symbol "[]"
  void $ symbol "=>"
  e2 <- expr
  x <- identifier
  void $ symbol ":"
  xs <- identifier
  void $ symbol "=>"
  ECase pos e1 e2 (x, xs) <$> expr

eLet :: Parser (Expr SourcePos)
eLet = do
  pos <- getSourcePos
  rword "let"
  x <- identifier
  void $ symbol "="
  e1 <- expr
  rword "in"
  ELet pos x e1 <$> expr

eLetTuple :: Parser (Expr SourcePos)
eLetTuple = do
  pos <- getSourcePos
  rword "let"
  xs <- parens (sepBy identifier $ symbol ",")
  void $ symbol "="
  e1 <- expr
  rword "in"
  ELetTuple pos xs e1 <$> expr

eTuple :: Parser (Expr SourcePos)
eTuple = parens (ETuple <$> getSourcePos <*> sepBy expr comma)

eApp :: Parser (Expr SourcePos)
eApp = do
  pos <- getSourcePos
  f <- eSimple
  args <- some eTuple
  return $ foldl (EApp pos) f args

eList :: Parser (Expr SourcePos)
eList = do
  pos <- getSourcePos
  es <- brackets (sepBy expr comma)
  return $ foldr (EBinOp pos (BinOp ":")) (ENil pos) es

eAction :: Parser (Expr SourcePos)
eAction = EAction <$> getSourcePos <*> upperIdentifier <*> eTuple

eHandle :: Parser (Expr SourcePos)
eHandle = do
  pos <- getSourcePos
  rword "handle"
  effName <- angles upperIdentifier
  e <- parens expr
  cs <- braces $ sepBy (clause <|> returnClause) comma
  option () (void comma)
  return $ EHandle pos effName e cs

clause :: Parser (Clause SourcePos)
clause = do
  pos <- getSourcePos
  name <- upperIdentifier
  args <- parens $ sepBy identifier comma
  void $ symbol "=>"
  Clause pos name args <$> expr

returnClause :: Parser (Clause SourcePos)
returnClause = do
  pos <- getSourcePos
  name <- symbol "return"
  arg <- parens identifier
  void $ symbol "=>"
  Clause pos name [arg] <$> expr

effectDef :: Parser (EffectDef SourcePos)
effectDef = do
  pos <- getSourcePos
  rword "effect"
  name <- upperIdentifier
  EffectDef pos name <$> braces (some actionDef)

actionDef :: Parser (ActionDef SourcePos)
actionDef = do
  pos <- getSourcePos
  name <- upperIdentifier
  args <- parens $ sepBy typeParser comma
  annot <- option Nothing (symbol "::" >> Just <$> typeParser)
  return $ ActionDef pos name args annot

typeParser :: Parser Type
typeParser =  try tArrow <|> tSimple

tArrow :: Parser Type
tArrow = do
  ta <- TProduct <$> parens (sepBy typeParser (symbol ","))
  void $ symbol "->"
  effects <- option EffEmpty effectRow
  TArrow ta effects <$> typeParser

tSimple :: Parser Type
tSimple  =
  (rword "Bool" >> return TBool) <|>
  (rword "Int" >> return TInt) <|>
  (rword "String" >> return TString) <|>
  brackets typeParser <|>
  tProduct

tProduct :: Parser Type
tProduct = parens
  (do
   ts <- sepBy1 typeParser comma
   case ts of
     [t] -> return t
     _ -> return $ TProduct ts)

effectRow :: Parser EffectRow
effectRow = angles (do
  labels <- sepBy upperIdentifier $ symbol ","
  return $ foldr EffLabel EffEmpty labels)