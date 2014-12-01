module Syntax.Small.Parser where

import Syntax.Location
import Syntax.Module
import Syntax.Expression
import Syntax.Name
import Syntax.Small.Token

import Control.Monad.Except
import Control.Monad.State
import Control.Applicative

import Data.Char
import Data.Foldable (foldrM)

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

data ParseError = ParseError
                | EmptyLambda Location
                | EmptyLet Location
                | EmptyDo Location
                | DuplicateBinding SyntaxName
                | DuplicateCasePattern SyntaxName
                | ExpectedToken Token' Token
                | ExpectedIdentifier0 Token
                | ExpectedIdentifier1 Token
                | ExpectedInteger Token
                | ExpectedString Token
                | ExpectedChar Token
                | EquationDifferentArguments SyntaxName Location [SyntaxName] [SyntaxName]
                | UnexpectedToken Token
                deriving (Show)


showParseError :: String -> ParseError -> String
showParseError fn (EmptyLambda l)         = showLocation fn l ++ "\tMissing lambda variables."
showParseError fn (EmptyLet l)            = showLocation fn l ++ "\tEmpty let bindings block."
showParseError fn (EmptyDo l)             = showLocation fn l ++ "\tEmpty do block."
showParseError fn (ExpectedToken t t')    = showLocation fn (locate t') ++ "\tExpected token " ++ show t ++ ", got " ++ show (delocate t') ++ "."
showParseError fn (ExpectedIdentifier0 t) = showLocation fn (locate t) ++ "\tExpected identifier_0 token, got " ++ show (delocate t) ++ "."
showParseError fn (ExpectedIdentifier1 t) = showLocation fn (locate t) ++ "\tExpected identifier_1 token, got " ++ show (delocate t) ++ "."
showParseError fn (ExpectedInteger t)     = showLocation fn (locate t) ++ "\tExpected integer token, got " ++ show (delocate t) ++ "."
showParseError fn (ExpectedString t)      = showLocation fn (locate t) ++ "\tExpected string token, got " ++ show (delocate t) ++ "."
showParseError fn (ExpectedChar t)        = showLocation fn (locate t) ++ "\tExpected char token, got " ++ show (delocate t) ++ "."
showParseError fn (UnexpectedToken t)     = showLocation fn (locate t) ++ "\tUnexpected token " ++ show t ++ "."
showParseError fn e                       = show e -- ...


data ParseState = ParseState
  { parserInput    :: [Token]
  }

-- Parser

type Parser = StateT ParseState (Except ParseError)

runParser :: [Token] -> Parser a -> Either ParseError a
runParser ts p = runExcept $ flip evalStateT (ParseState ts) p

nextToken :: Parser Token
nextToken = do
  input <- parserInput <$> get
  case input of
    []    -> return (Locate (Location startPosition 0) TkEOF)
    tok : _ -> return tok

consumeToken :: Parser ()
consumeToken = 
  modify $ \s -> case parserInput s of
    []                    -> s
    (Locate _ TkEOF) : [] -> s
    c : cs                -> s { parserInput = cs
                               }

getPosition :: Parser Position
getPosition = do
  Location p _ <- locate <$> nextToken
  return p

-- Parsing

parseMany :: Parser a -> Parser Bool -> Parser [a]
parseMany p c = do
  b <- c
  if b
    then do
      a  <- p
      as <- parseMany p c
      return (a : as)
    else
      return []

parseManySep :: Parser a -> Parser b -> Parser Bool -> Parser Bool -> Parser [a]
parseManySep p s c cs = do
  b <- c
  if b
    then do
      a <- p
      as <- parseMany (s >> p) cs
      return (a : as)
    else
      return []

parseManySepOpt :: Parser a -> Parser b -> Parser Bool -> Parser Bool -> Parser [a]
parseManySepOpt p s c cs = do
  b <- c
  if b
    then do
      a <- p
      b <- cs
      if b 
        then do
          s
          as <- parseManySepOpt p s c cs
          return (a : as)
        else
          return [a]
    else do
      return []

parseMany1 :: Parser a -> Parser Bool -> ParseError -> Parser [a]
parseMany1 p c e = do
  b <- c
  if b
    then do
      a  <- p
      as <- parseMany p c
      return (a : as)
    else
      throwError e

parseManySepOpt1 :: Parser a -> Parser b -> Parser Bool -> Parser Bool -> ParseError -> Parser [a]
parseManySepOpt1 p s c cs e = do
  b <- c
  if b
    then do
      a <- p
      b <- cs
      if b 
        then do
          s
          as <- parseManySepOpt p s c cs
          return (a : as)
        else
          return [a]
    else
      throwError e

parseManySep1 :: Parser a -> Parser b -> Parser Bool -> Parser Bool -> ParseError -> Parser [a]
parseManySep1 p s c cs e = do
  b <- c
  if b
    then do
      a <- p
      s
      as <- parseMany (s >> p) cs
      return (a : as)
    else
      throwError e

-- Bindings

makeBindings :: [Locate (SyntaxName, [SyntaxName], Expression SyntaxName)] -> Parser (DeclarationMap SyntaxName)
makeBindings =
  foldrM
    (\(Locate loc (name, xs, e)) acc -> do
      case Map.lookup name acc of
        Nothing ->
          return $ Map.insert name (Declaration Nothing $ foldr (\x -> Locate loc . ELambda x) e xs) acc
        Just _  -> throwError $ DuplicateBinding name
    )
    Map.empty

-- Module

parseModule :: Parser (Module SyntaxName)
parseModule = do
  bs <- makeBindings =<< parseMany parseDefinition (isNotToken TkEOF)
  expectToken TkEOF
  return $ Module ["Main"] (Set.singleton ["Primitive"]) Map.empty Map.empty Map.empty bs

parseDefinition :: Parser (Binding SyntaxName)
parseDefinition = do
  p1 <- getPosition
  name <- UserName <$> parseIdentifier0
  xs <- parseMany (UserName <$> parseIdentifier1) (isNotToken TkDefine)
  expectToken TkDefine
  e <- parseExpression
  p2 <- getPosition
  return $ Locate (makeLocation p1 p2) (name, xs, e)

-- Expressions

data Precedence = LPrec | RPrec

makePrecedenceParser :: [(Precedence, [Token'])] -> Parser (Expression SyntaxName) -> Parser (Expression SyntaxName)
makePrecedenceParser [] p                = p
makePrecedenceParser ((prec, ts) : xs) p =
  let nextParser = makePrecedenceParser xs p in do
    p1 <- getPosition
    e1 <- nextParser
    op <- nextToken
    if delocate op `elem` ts
      then do
        consumeToken
        e2 <- makePrecedenceParser ((prec, ts) : xs) p
        p2 <- getPosition
        return $ Locate (makeLocation p1 p2) $
          EApplication
            (Locate (makeLocation p1 p2) $
              EApplication
                (Locate (locate op) $
                  EVariable (QName [] (precedenceParserOperatorNamespace $ delocate op) (UserName . precedenceParserOperator $ delocate op))
                )
                e1
            )
            e2
      else return e1
  where
    precedenceParserOperator TkOr      = "||"
    precedenceParserOperator TkAnd     = "&&"
    precedenceParserOperator TkLT      = "<"
    precedenceParserOperator TkLE      = "<="
    precedenceParserOperator TkGT      = ">"
    precedenceParserOperator TkGE      = ">="
    precedenceParserOperator TkEQ      = "=="
    precedenceParserOperator TkNE      = "/="
    precedenceParserOperator TkColon   = ":"
    precedenceParserOperator TkPlus    = "+"
    precedenceParserOperator TkMinus   = "-"
    precedenceParserOperator TkTimes   = "*"
    precedenceParserOperator TkDivides = "/"

    precedenceParserOperatorNamespace TkColon = ConstructorName
    precedenceParserOperatorNamespace _       = VariableName

precedences :: [(Precedence, [Token'])]
precedences =
  [ (LPrec, [TkOr])
  , (LPrec, [TkAnd])
  , (LPrec, [TkLT, TkLE, TkGT, TkGE, TkEQ, TkNE])
  , (RPrec, [TkColon])
  , (LPrec, [TkPlus, TkMinus])
  , (LPrec, [TkTimes, TkDivides])
  ]

parseSimpleExpression :: Parser (Expression SyntaxName)
parseSimpleExpression = do
  t <- nextToken
  p1 <- getPosition
  case delocate t of
    TkLParen         -> parseTuple
    TkLBracket       -> parseList
    TkTrue           -> do
      consumeToken
      p2 <- getPosition
      return $ Locate (makeLocation p1 p2) (EVariable (QName [] ConstructorName (UserName "True")))
    TkFalse          -> do
      consumeToken
      p2 <- getPosition
      return $ Locate (makeLocation p1 p2) (EVariable (QName [] ConstructorName (UserName "False")))
    TkIdentifier1 id -> do
      consumeToken
      p2 <- getPosition
      return $ Locate (makeLocation p1 p2) (EVariable (QName [] VariableName (UserName id)))
    TkInteger i      -> do
      consumeToken
      p2 <- getPosition
      return $ Locate (makeLocation p1 p2) (EInteger i)
    TkString s       -> do
      consumeToken
      p2 <- getPosition
      let nil = Locate (makeLocation p1 p2) (EVariable (QName [] ConstructorName (UserName "[]")))
      let cons = Locate (makeLocation p1 p2) (EVariable (QName [] ConstructorName (UserName ":")))
      let makeCons a b = Locate (makeLocation p1 p2) $
            EApplication
              (Locate (makeLocation p1 p2) $ EApplication cons a)
              b
      return $ foldr (\c -> makeCons $ Locate (makeLocation p1 p2) $ EChar c) nil s
    TkChar c         -> do
      consumeToken
      p2 <- getPosition
      return $ Locate (makeLocation p1 p2) $ EChar c
    _ -> throwError $ UnexpectedToken t

parseApplications :: Parser (Expression SyntaxName)
parseApplications = do
  p1 <- getPosition
  (e:es) <- parseMany1 parseSimpleExpression (satisfy isSimpleExpressionStart) ParseError
  p2 <- getPosition
  let makeApplication f t      = Locate (makeLocation p1 p2) (EApplication f t)
      makeApplicationList f ts = foldl makeApplication f ts
  return $ makeApplicationList e es

parseNegation :: Parser (Expression SyntaxName)
parseNegation = do
  t <- nextToken
  p1 <- getPosition
  case delocate t of
    TkMinus -> do
      consumeToken
      p2 <- getPosition
      e  <- parseApplications
      p3 <- getPosition
      return $ Locate (makeLocation p1 p3) $
        EApplication
          (Locate (makeLocation p1 p2) (EVariable (QName [] VariableName (UserName "negate"))))
          e
    _       -> parseApplications

precedenceParser :: Parser (Expression SyntaxName)
precedenceParser = makePrecedenceParser precedences parseNegation

isSimpleExpressionStart :: Token' -> Bool
isSimpleExpressionStart TkLParen          = True
isSimpleExpressionStart TkTrue            = True
isSimpleExpressionStart TkFalse           = True
isSimpleExpressionStart TkLBracket        = True
isSimpleExpressionStart (TkIdentifier1 _) = True
isSimpleExpressionStart (TkInteger _)     = True
isSimpleExpressionStart (TkString _)      = True
isSimpleExpressionStart (TkChar _)        = True
isSimpleExpressionStart _                 = False

isExpressionStart :: Token' -> Bool
isExpressionStart TkLambda          = True
isExpressionStart TkIf              = True
isExpressionStart TkLet             = True
isExpressionStart TkCase            = True
isExpressionStart TkDo              = True
isExpressionStart TkMinus           = True
isExpressionStart TkLParen          = True
isExpressionStart TkTrue            = True
isExpressionStart TkFalse           = True
isExpressionStart TkLBracket        = True
isExpressionStart (TkIdentifier1 _) = True
isExpressionStart (TkInteger _)     = True
isExpressionStart (TkString _)      = True
isExpressionStart (TkChar _)        = True
isExpressionStart _                 = False

parseExpression :: Parser (Expression SyntaxName)
parseExpression = do
  t <- nextToken
  case delocate t of
    TkLambda -> parseLambda
    TkIf     -> parseIf
    TkLet    -> parseLet
    TkCase   -> parseCase
    TkDo     -> parseDo
    _        -> precedenceParser

parseLambda :: Parser (Expression SyntaxName)
parseLambda = do
  p1 <- getPosition
  l <- expectToken TkLambda
  xs <- parseMany1 parseIdentifier1 (isNotToken TkArrow) (EmptyLambda l)
  expectToken TkArrow
  e <- parseExpression
  p2 <- getPosition
  let loc = makeLocation p1 p2
  return $ foldr (\x -> Locate loc . ELambda (UserName x)) e xs

parseIf :: Parser (Expression SyntaxName)
parseIf = do
  p1 <- getPosition
  expectToken TkIf
  c <- parseExpression
  expectToken TkThen
  a <- parseExpression
  expectToken TkElse
  b <- parseExpression
  p2 <- getPosition
  return $ Locate (makeLocation p1 p2) $
    ECase c
      [ (Pattern (PConstructor (QName ["Primitive"] ConstructorName (UserName "True")) []) [], a)
      , (Pattern (PConstructor (QName ["Primitive"] ConstructorName (UserName "False")) []) [], b)
      ]

parseLet :: Parser (Expression SyntaxName)
parseLet = do
  p1 <- getPosition
  l <- expectToken TkLet
  t <- delocate <$> nextToken
  bs <- makeBindings =<< case t of
    TkLBrace -> do
      expectToken TkLBrace
      bs <- parseManySepOpt1 parseBinding (expectToken TkSemiColon) isIdentifier1 (isToken TkSemiColon) (EmptyLet l)
      maybeToken TkSemiColon
      expectToken TkRBrace
      return bs
    _        -> (: []) <$> parseBinding
  expectToken TkIn
  e <- parseExpression
  p2 <- getPosition
  return $ Locate (makeLocation p1 p2) (ELet bs e)

parseCase :: Parser (Expression SyntaxName)
parseCase = do
  p1 <- getPosition
  expectToken TkCase
  e <- parseExpression
  expectToken TkOf
  expectToken TkLBrace
  expectToken TkLBracket
  expectToken TkRBracket
  expectToken TkArrow
  c1 <- parseExpression
  expectToken TkSemiColon
  x <- UserName <$> parseIdentifier1
  expectToken TkColon
  xs <- UserName <$> parseIdentifier1
  expectToken TkArrow
  c2 <- parseExpression
  maybeToken TkSemiColon
  expectToken TkRBrace
  p2 <- getPosition
  when (x == xs) (throwError $ DuplicateCasePattern x)
  return $ Locate (makeLocation p1 p2) $
    ECase e
      [ (Pattern (PConstructor (QName ["Primitive"] ConstructorName (UserName "[]")) []) [], c1)
      , (Pattern (PConstructor (QName ["Primitive"] ConstructorName (UserName ":")) [Pattern PWildcard [x], Pattern PWildcard [xs]]) [], c2)
      ]

parseDo :: Parser (Expression SyntaxName)
parseDo = do
  p1 <- getPosition
  l <- expectToken TkDo
  expectToken TkLBrace
  as <- parseManySepOpt1 parseExpression (expectToken TkSemiColon) (satisfy isExpressionStart) (isToken TkSemiColon) (EmptyDo l)
  maybeToken TkSemiColon
  expectToken TkRBrace
  p2 <- getPosition
  let mkThen a b = Locate (makeLocation p1 p2) $
        EApplication
          (Locate (makeLocation p1 p2) $
            EApplication
            (Locate (makeLocation p1 p2) $ EVariable (QName [] VariableName (UserName ">>=")))
            a
          ) 
          (Locate (locate b) $ ELambda (UserName "") b)
  return $ foldr1 mkThen as

parseBinding :: Parser (Binding SyntaxName)
parseBinding = do
  p1 <- getPosition
  name <- UserName <$> parseIdentifier1
  xs <- parseMany (UserName <$> parseIdentifier1) (isNotToken TkDefine)
  expectToken TkDefine
  e <- parseExpression
  p2 <- getPosition
  return $ Locate (makeLocation p1 p2) (name, xs, e)

parseTuple :: Parser (Expression SyntaxName)
parseTuple = do
  p1 <- getPosition
  expectToken TkLParen
  xs <- parseManySep parseExpression (expectToken TkComma) (satisfy isExpressionStart) (isToken TkComma)
  expectToken TkRParen
  p2 <- getPosition
  case xs of
    []   -> return $ Locate (makeLocation p1 p2) (EVariable (QName [] ConstructorName (UserName "()")))
    x:[] -> return x
    _    -> do
      let makeApplication f t      = Locate (makeLocation p1 p2) (EApplication f t)
          makeApplicationList f ts = foldl makeApplication f ts
          tupleConstructor         = Locate (makeLocation p1 p2) (EVariable (QName [] ConstructorName (UserName (replicate (length xs - 1) ','))))
      return $ makeApplicationList tupleConstructor xs

parseList :: Parser (Expression SyntaxName)
parseList = do
  p1 <- getPosition
  expectToken TkLBracket
  xs <- parseManySep parseExpression (expectToken TkComma) (satisfy isExpressionStart) (isToken TkComma)
  expectToken TkRBracket
  p2 <- getPosition
  let nil = Locate (makeLocation p1 p2) (EVariable (QName [] ConstructorName (UserName "[]")))
  let cons = Locate (makeLocation p1 p2) (EVariable (QName [] ConstructorName (UserName ":")))
  let makeCons a b = Locate (makeLocation p1 p2) $
        EApplication
          (Locate (makeLocation p1 p2) $ EApplication cons a)
          b
  return $ foldr makeCons nil xs

-- Tokens

parseIdentifier0 :: Parser String
parseIdentifier0 = do
  t <- nextToken
  consumeToken
  case delocate t of
    TkIdentifier0 s -> return s
    _               -> throwError $ ExpectedIdentifier0 t

parseIdentifier1 :: Parser String
parseIdentifier1 = do
  t <- nextToken
  consumeToken
  case delocate t of
    TkIdentifier1 s -> return s
    _               -> throwError $ ExpectedIdentifier1 t

expectToken :: Token' -> Parser Location
expectToken t = do
  t' <- nextToken
  when (t /= delocate t') (throwError $ ExpectedToken t t')
  consumeToken
  return (locate t')

maybeToken :: Token' -> Parser ()
maybeToken t = do
  t' <- nextToken
  when (t == delocate t') consumeToken

isToken :: Token' -> Parser Bool
isToken = satisfy . (==)

isIdentifier0 :: Parser Bool
isIdentifier0 = satisfy (\x -> case x of { TkIdentifier0 _ -> True ; _ -> False })

isIdentifier1 :: Parser Bool
isIdentifier1 = satisfy (\x -> case x of { TkIdentifier1 _ -> True ; _ -> False })

isNotToken :: Token' -> Parser Bool
isNotToken = satisfy . (/=)

satisfy :: (Token' -> Bool) -> Parser Bool
satisfy f = f . delocate <$> nextToken
