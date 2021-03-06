module LambdaCore.Parser where

import LambdaCore.Data

import Text.ParserCombinators.Parsec
import qualified Control.Applicative as A
import qualified Data.Set as S
import Control.Monad

-- $setup
-- >>> import Data.Either

-- | Skip one or more spaces.
skipSpaces1 :: Parser ()
skipSpaces1 = skipMany1 space

-- | Parse string.
--
-- >>> parse parseString "" "\"Hello\""
-- Right (Lit (StringV "Hello"))
--
-- >>> parse parseString "" "\"\""
-- Right (Lit (StringV ""))
--
-- >>> isLeft $ parse parseString "" "Hello"
-- True
--
-- >>> isLeft $ parse parseString "" "\"Hello"
-- True
--
parseString :: Parser Exp
parseString = do
  s <- between (char '"') (char '"') (many (noneOf "\""))
  return $ Lit (StringV s)

-- | Parse Boolean.
--
-- >>> parse parseBool "" "true"
-- Right (Lit (BoolV True))
--
-- >>> parse parseBool "" "false"
-- Right (Lit (BoolV False))
--
-- >>> isLeft $ parse parseBool "" "\"true\""
-- True
--
parseBool :: Parser Exp
parseBool = do
  -- 'string' consumes input, so use 'try' so that no input is consumed if the
  -- literal is not true or false. This way, variables that are prefixed with
  -- any prefix of "true" or "false" can be parsed (e.g. 'fx' or 'true-thing').
  s <- try (string "true") <|> try (string "false")
  return $ Lit (BoolV (s == "true"))

-- | Parse Integers.
--
-- >>> parse parseInt "" "0"
-- Right (Lit (IntV 0))
--
-- >>> parse parseInt "" "123"
-- Right (Lit (IntV 123))
--
-- >>> parse parseInt "" "123.456"
-- Right (Lit (IntV 123))
--
-- >>> isLeft $ parse parseInt "" ".456"
-- True
--
parseInt :: Parser Exp
parseInt = do
  s <- many1 digit
  return $ Lit (IntV (read s :: Int))

-- | Parse variable identifier. Identifiers must be prefixed with either a
-- letter, dash (-) or underscore (_). Or a reserved symbol identifier.
--
-- >>> parse parseVar "" "x"
-- Right (Var "x")
--
-- >>> parse parseVar "" "abc-def"
-- Right (Var "abc-def")
--
-- >>> parse parseVar "" ">>="
-- Right (Var ">>=")
--
-- >>> parse parseVar "" "-"
-- Right (Var "-")
--
-- >>> isLeft $ parse parseVar "" "_"
-- True
--
-- >>> isLeft $ parse parseVar "" "fn"
-- True
--
parseVar :: Parser Exp
parseVar = do
  i <- try parseAlphaNumericId <|> parseSymbolId
  if S.member i reservedIds
    then unexpected $ "reserved identifier '" ++ i ++ "'"
    else return $ Var i

-- | Parse an alpha-numeric identifier.
--
-- >>> parse parseAlphaNumericId "" "abc"
-- Right "abc"
--
-- >>> parse parseAlphaNumericId "" "_abc"
-- Right "_abc"
--
-- >>> parse parseAlphaNumericId "" "-abc_def-123-"
-- Right "-abc_def-123-"
--
-- >>> isLeft $ parse parseAlphaNumericId "" "-"
-- True
--
-- >>> isLeft $ parse parseAlphaNumericId "" "-123"
-- True
--
-- >>> isLeft $ parse parseAlphaNumericId "" "_"
-- True
--
-- >>> isLeft $ parse parseAlphaNumericId "" "123abc"
-- True
--
parseAlphaNumericId :: Parser String
parseAlphaNumericId = do
  -- Get first character.
  p <- letter <|> char '-' <|> char '_'

  -- If first character was a - or _, then needs to be followed one letter, then
  -- zero or more alphanumerics (e.g. -123 is invalid, but -abc is valid).
  --
  -- Else, it can be followed by zero or more alphanumerics.
  --
  -- TODO: How to simplify this?
  if p == '-' || p == '_'
    then (do
      p' <- letter
      rest <- restOfAlphaNumericId
      return (p : p' : rest))
    else (do
      rest <- restOfAlphaNumericId
      return (p : rest))

restOfAlphaNumericId :: Parser String
restOfAlphaNumericId = many (alphaNum <|> char '-' <|> char '_') <?> "Non-symbolic identifier must consist of alphanumeric characters, dashes and underscores."

-- | Parse a symbol-only identifier.
--
-- >>> parse parseSymbolId "" ">= abc"
-- Right ">="
--
-- >>> parse parseSymbolId "" "+"
-- Right "+"
--
-- >>> parse parseSymbolId "" "+++++"
-- Right "+++++"
--
-- Bind operator.
-- >>> parse parseSymbolId "" ">>="
-- Right ">>="
--
-- >>> parse parseSymbolId "" "+3"
-- Right "+"
--
-- >>> isLeft $ parse parseSymbolId "" "!!!"
-- True
--
parseSymbolId :: Parser String
parseSymbolId = many1 symbolIds -- ID must be one or more symbols

-- | Symbols that can be part of symbol-only identifiers.
symbolIds :: Parser Char
symbolIds = oneOf validIdSymbols

-- | Parse many @p@s, separated by at least one space.
parseMany :: Parser a -> Parser [a]
parseMany p = sepBy p skipSpaces1

parseExps :: Parser [Exp]
parseExps = parseMany parseExp

parseListWithSurroundingPrefix ::
  Maybe (Parser String)
  -- ^ Optional prefix parser
  -> Char -> Char
  -- ^ Start and begin char
  -> Parser [a]
  -- Parse multiple of these things
  -> ([a] -> a)
  -- Convert multiple things into one
  -> Parser a
parseListWithSurroundingPrefix mp l r ps f = do
  _ <- char l
  case mp of
    Just s -> s A.*> skipSpaces1
    _      -> spaces

  -- Must be separated by at least one space
  exps <- ps

  _ <- char r
  return $ f exps

parseListWithSurrounding :: Char -> Char -> Parser [a] -> ([a] -> a) -> Parser a
parseListWithSurrounding = parseListWithSurroundingPrefix Nothing

-- | Parse unary function calls.
--
-- >>> parse parseApp "" "(x 123)"
-- Right (App (Var "x") (Lit (IntV 123)))
--
-- Curry (x 1 2) as ((x 1) 2):
--
-- >>> parse parseApp "" "(x 1 2)"
-- Right (App (App (Var "x") (Lit (IntV 1))) (Lit (IntV 2)))
--
-- >>> parse parseApp "" "(x 1 2 3)"
-- Right (App (App (App (Var "x") (Lit (IntV 1))) (Lit (IntV 2))) (Lit (IntV 3)))
--
-- >>> parse parseApp "" "((fn (x y z) (+ x y)) 1 2 3)"
-- Right (App (App (App (Fun [Var "x",Var "y",Var "z"] (App (App (Var "+") (Var "x")) (Var "y"))) (Lit (IntV 1))) (Lit (IntV 2))) (Lit (IntV 3)))
--
parseApp :: Parser Exp
parseApp = try $ do
  _ <- char '('
  varOrFn <- parseExp
  skipSpaces1
  args <- parseExps
  _ <- char ')'
  return $ buildAppStack varOrFn args

-- | Convert a function call with multiple arguments to recursive unary calls.
-- If no arguments are given, return a nullary function call.
--
-- >>> buildAppStack (Var "x") [Lit (IntV 1)]
-- App (Var "x") (Lit (IntV 1))
--
-- >>> buildAppStack (Var "x") [Lit (IntV 1),Lit (IntV 2),Lit (IntV 3)]
-- App (App (App (Var "x") (Lit (IntV 1))) (Lit (IntV 2))) (Lit (IntV 3))
--
buildAppStack :: Exp -> [Exp] -> Exp
buildAppStack _ [] = error "Function application must contain argument."
buildAppStack fn [arg] = App fn arg
buildAppStack fn (a:as) = buildAppStack (App fn a) as

-- | Parse functions.
--
-- >>> parse parseFun "" "(fn (x y z) (x y z))"
-- Right (Fun [Var "x",Var "y",Var "z"] (App (App (Var "x") (Var "y")) (Var "z")))
--
-- >>> isLeft $ parse parseFun "" "(fn () 3)"
-- True
--
parseFun :: Parser Exp
parseFun = do
  parseStartsListWith "fn"
  argsVec <- parseFunArgs1
  body <- parseBodyOfFun
  return $ Fun argsVec body

parseStartsListWith :: String -> Parser ()
parseStartsListWith keyword = do
  _ <- char '('
  _ <- try $ string keyword
  skipSpaces1

parseBodyOfFun :: Parser Exp
parseBodyOfFun = do
  _ <- skipSpaces1
  body <- parseExp
  _ <- char ')'
  return body

-- | Parse vector of vars.
--
-- >>> isLeft $ parse parseFunArgs1 "" "()"
-- True
--
-- >>> parse parseFunArgs1 "" "(x y z)"
-- Right [Var "x",Var "y",Var "z"]
--
parseFunArgs1 :: Parser [Exp]
parseFunArgs1 = do
  _ <- char '('
  vars <- sepBy1 parseVar skipSpaces1
  _ <- char ')'
  return vars

-- | Parse let expressions.
--
-- >>> parse parseLet "" "(let (x 1 y 2) (+ x y))"
-- Right (Let (Var "x") (Lit (IntV 1)) (Let (Var "y") (Lit (IntV 2)) (App (App (Var "+") (Var "x")) (Var "y"))))
--
-- >>> isLeft $ parse parseLet "" "(let () (+ x y))"
-- True
--
-- >>> isLeft $ parse parseLet "" "(let (x 1 y) (+ x y))"
-- True
--
parseLet :: Parser Exp
parseLet = do
  parseStartsListWith "let"
  bindings <- parseVarExpPairs
  body <- parseBodyOfFun
  return $ buildLetBindingStack body bindings

buildLetBindingStack :: Exp -> [(Exp, Exp)] -> Exp
buildLetBindingStack _ [] = error "let must bind variables"
buildLetBindingStack body [(Var v,bind)] = Let (Var v) bind body
buildLetBindingStack body ((Var v,bind):bindings) = Let (Var v) bind (buildLetBindingStack body bindings)
buildLetBindingStack _ _ = error "let has invalid bindings"

-- | Parse (var, exp) pairs. Must have at least one.
--
-- >>> parse parseVarExpPairs "" "(x 1 y 2)"
-- Right [(Var "x",Lit (IntV 1)),(Var "y",Lit (IntV 2))]
--
-- >>> isLeft $ parse parseLet "" "()"
-- True
--
-- >>> isLeft $ parse parseLet "" "(x 1 y)"
-- True
--
parseVarExpPairs :: Parser [(Exp, Exp)]
parseVarExpPairs = do
  _ <- char '('
  pairs <- sepBy1 parseVarExpPair skipSpaces1
  _ <- char ')'
  return pairs

parseVarExpPair :: Parser (Exp, Exp)
parseVarExpPair = do
  var <- parseVar
  _ <- skipSpaces1
  body <- parseExp
  return (var, body)

-- | Parse if.
--
-- >>> parse parseIf "" "(if true (x 1) (y 1))"
-- Right (If (Lit (BoolV True)) (App (Var "x") (Lit (IntV 1))) (App (Var "y") (Lit (IntV 1))))
--
parseIf :: Parser Exp
parseIf = do
  parseStartsListWith "if"
  p <- parseExp
  _ <- skipSpaces1
  t <- parseExp
  e <- parseBodyOfFun
  return (If p t e)

-- | Parse expression.
--
-- >>> parse parseExp "" "\"abc\""
-- Right (Lit (StringV "abc"))
--
-- >>> parse parseExp "" "true"
-- Right (Lit (BoolV True))
--
-- >>> parse parseExp "" "123"
-- Right (Lit (IntV 123))
--
-- >>> parse parseExp "" "x"
-- Right (Var "x")
--
-- >>> parse parseExp "" "+ 13"
-- Right (Var "+")
--
-- >>> parse parseExp "" "(foo bar)"
-- Right (App (Var "foo") (Var "bar"))
--
-- >>> isLeft $ parse parseExp "" "(foo)"
-- True
--
-- >>> parse parseExp "" "(let (x 1 y 2) (+ x y))"
-- Right (Let (Var "x") (Lit (IntV 1)) (Let (Var "y") (Lit (IntV 2)) (App (App (Var "+") (Var "x")) (Var "y"))))
--
-- >>> parse parseExp "" "(let (x 1 y 2) (if true x y))"
-- Right (Let (Var "x") (Lit (IntV 1)) (Let (Var "y") (Lit (IntV 2)) (If (Lit (BoolV True)) (Var "x") (Var "y"))))
--
parseExp :: Parser Exp
parseExp =
  try parseString <|>
  try parseBool <|>
  try parseInt <|>
  try parseIf <|>
  try parseLet <|>
  try parseApp <|>
  try parseFun <|>
  try parseVar

-- | Parse a line of expression.
--
-- >>> isLeft $ parse parseLine "" "+ 13"
-- True
--
parseLine :: Parser Exp
parseLine = do
  expr <- parseExp

  -- Expect spaces and EOF after expression.
  _ <- many space
  _ <- eof

  return expr

-- | Parse a program.
--
-- >>> parseProgram "(+ 1 2)"
-- Right (App (App (Var "+") (Lit (IntV 1))) (Lit (IntV 2)))
--
-- >>> parseProgram "(fn (x y) (x y))"
-- Right (Fun [Var "x",Var "y"] (App (Var "x") (Var "y")))
--
-- >>> isLeft $ parseProgram "+ 13"
-- True
parseProgram :: Program -> Either ParseError Exp
parseProgram = parse parseLine ""

parseTString :: Parser Type
parseTString = string "String" >> return TString

parseTBool :: Parser Type
parseTBool = string "Bool" >> return TBool

parseTInt :: Parser Type
parseTInt = string "Int" >> return TInt

parseTFun :: Parser Type
parseTFun = parseListWithSurroundingPrefix (Just (string "->")) '(' ')' parseTypes TFun

parseTVar :: Parser Type
parseTVar = liftM TVar (many1 letter)

-- | Parse type.
--
-- >>> parse parseType "" "(-> a Int (-> a b String Bool) (-> Bool))"
-- Right (-> a Int (-> a b String Bool) (-> Bool))
--
parseType :: Parser Type
parseType =
  try parseTString <|>
  try parseTBool <|>
  try parseTInt <|>
  try parseTFun <|>
  try parseTVar

parseTypes :: Parser [Type]
parseTypes = parseMany parseType

