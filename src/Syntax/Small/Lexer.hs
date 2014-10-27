module Syntax.Small.Lexer where

import Control.Monad.Except
import Control.Monad.State
import Control.Applicative

import Syntax.Location
import Syntax.Small.Token

import Data.Char

data LexError = LexError Position
              | UnknownSymbol Location String
              | ExpectedChar Position Char Char
              | BadStringCharacter Position Char
              | BadStringEscape Position Char
              | UnterminatedComment
              deriving (Show)

data LexState = LexState
  { lexerPosition :: Position
  , lexerInput    :: String
  }

-- Lexer

type Lexer = StateT LexState (Except LexError)

currentPosition :: Lexer Position
currentPosition = lexerPosition <$> get

nextChar :: Lexer Char
nextChar = do
  input <- lexerInput <$> get
  case input of
    []    -> return '\0'
    c : _ -> return c

consumeChar :: Lexer ()
consumeChar = 
  modify $ \s -> case lexerInput s of
    []     -> s
    c : cs -> s { lexerPosition = updatePosition (lexerPosition s) c
                , lexerInput    = cs
                }

-- Lexing

tokenise :: String -> Either LexError [Token]
tokenise cs = runExcept $ flip evalStateT (LexState startPosition cs) $ tokenise'
  where
    tokenise' = do
      c <- nextChar
      if c == '\0'
        then return []
        else do
          t  <- lexToken
          ts <- tokenise'
          return (t : ts)

symbolChar :: [Char]
symbolChar = "+-*/<>=:&|"

symbols :: [(String, Token')]
symbols  = 
  [ ("->", TkArrow)
  , ("=",  TkDefine)
  , ("||", TkOr)
  , ("&&", TkAnd)
  , ("<",  TkLT)
  , ("<=", TkLE)
  , (">",  TkGT)
  , (">=", TkGE)
  , ("==", TkEQ)
  , ("/=", TkNE)
  , (":",  TkColon)
  , ("+",  TkPlus)
  , ("-",  TkMinus)
  , ("*",  TkTimes)
  , ("/",  TkDivides)
  ]

keywords :: [(String, Token')]
keywords =
  [ ("if", TkIf)
  , ("then", TkThen)
  , ("else", TkElse)
  , ("let", TkLet)
  , ("in", TkIn)
  , ("case", TkCase)
  , ("of", TkOf)
  , ("do", TkDo)
  ]

lexToken :: Lexer Token
lexToken = do
  c <- nextChar
  if isSpace c
    then consumeChar >> lexToken
    else do
      p <- currentPosition
      tok <- case c of
        '\0'                    -> return TkEOF
        '\"'                    -> TkString <$> lexString
        '\''                    -> TkChar <$> lexChar
        '('                     -> consumeChar >> return TkLParen
        ')'                     -> consumeChar >> return TkRParen
        '{'                     -> do
          consumeChar
          c <- nextChar
          if c == '-'
            then (delocate <$>) $ consumeChar >> lexMultiLineComment >> lexToken
            else return TkLBrace
        '}'                     -> consumeChar >> return TkRBrace
        '['                     -> consumeChar >> return TkLBracket
        ']'                     -> consumeChar >> return TkRBracket
        ','                     -> consumeChar >> return TkComma
        ';'                     -> consumeChar >> return TkSemiColon
        '\\'                    -> consumeChar >> return TkLambda
        _ | c `elem` symbolChar -> lexSymbol
        _ | isLower c           -> lexIdentifier
        _ | isUpper c           -> lexBool
        _ | isDigit c           -> TkInteger <$> lexInteger
        _                       -> throwError (LexError p)
      p' <- currentPosition
      return $ Locate (makeLocation p p') tok

lexMultiLineComment :: Lexer ()
lexMultiLineComment = do
  c <- nextChar
  case c of
    '\0' -> throwError UnterminatedComment
    '-'  -> do
      consumeChar
      c <- nextChar
      if c == '}'
        then consumeChar
        else lexMultiLineComment
    _    -> consumeChar >> lexMultiLineComment

lexSingleLineComment :: Lexer ()
lexSingleLineComment = do
  c <- nextChar
  case c of
    '\0' -> return ()
    '\n' -> consumeChar
    _    -> consumeChar >> lexSingleLineComment

lexIdentifier :: Lexer Token'
lexIdentifier = do
    p   <- currentPosition
    str <- lexIdentifier'
    case lookup str keywords of
      Just t  ->
        return t
      Nothing ->
        if positionColumn p == 1
          then return $ TkIdentifier0 str
          else return $ TkIdentifier1 str
  where
    lexIdentifier' :: Lexer String
    lexIdentifier' = do
      c <- nextChar
      if isAlphaNum c || c `elem` "_'"
        then do
          consumeChar
          (c :) <$> lexIdentifier'
        else return ""

lexInteger :: Lexer Int
lexInteger = lexInteger' 0
  where
    lexInteger' :: Int -> Lexer Int
    lexInteger' i = do
      c <- nextChar
      if isDigit c
        then consumeChar >> lexInteger' (10 * i + fromIntegral (ord c - ord '0'))
        else return i

lexSymbol :: Lexer Token'
lexSymbol = do
    p   <- currentPosition
    str <- lexSymbol'
    case lookup str symbols of
      Nothing | str == "--" -> (delocate <$>) $ lexSingleLineComment >> lexToken
      Nothing               -> throwError $ UnknownSymbol (Location p (length str)) str
      Just tok              -> return tok
  where
    lexSymbol' = do
      c <- nextChar
      if c `elem` symbolChar
        then consumeChar >> (c :) <$> lexSymbol'
        else return ""

lexBool :: Lexer Token'
lexBool = do
    p   <- currentPosition
    str <- lexBool'
    case str of
      "True"  -> return TkTrue
      "False" -> return TkFalse
      _       -> throwError $ LexError p
  where
    lexBool' :: Lexer String
    lexBool' = do
      c <- nextChar
      if isAlphaNum c || c `elem` "_'"
        then do
          consumeChar
          (c :) <$> lexBool'
        else return ""

lexChar :: Lexer Char
lexChar = do
  expectChar '\''
  c <- stringChar
  expectChar '\''
  return c

expectChar :: Char -> Lexer ()
expectChar c = do
  c' <- nextChar
  p  <- currentPosition 
  when (c /= c') (throwError $ ExpectedChar p c c')
  consumeChar

stringChar :: Lexer Char
stringChar = do
  c <- nextChar
  p <- currentPosition
  case c of
    '\\'                            -> consumeChar >> escapeChar
    _ | 32 <= ord c && ord c <= 126 -> consumeChar >> return c
    _                               -> throwError $ BadStringCharacter p c

escapeChar :: Lexer Char
escapeChar = do
  c <- nextChar
  p <- currentPosition
  case c of
    '\\'                            -> consumeChar >> return '\\'
    '\"'                            -> consumeChar >> return '\"'
    'n'                             -> consumeChar >> return '\n'
    't'                             -> consumeChar >> return '\t'
    _                               -> throwError $ BadStringEscape p c

lexString :: Lexer String
lexString = do
    expectChar '\"'
    lexString'
  where
    lexString' = do
      c <- nextChar
      if c == '\"'
        then consumeChar >> return ""
        else do
          c  <- stringChar
          cs <- lexString'
          return (c : cs)
