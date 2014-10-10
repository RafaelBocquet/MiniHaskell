module Syntax.Full.Lexer where

import Control.Monad.Except
import Control.Monad.State
import Control.Applicative

import Syntax.Location
import Syntax.Full.Token

import Data.Char

-- TODO : Module.Id.(+)

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

tokenize :: String -> Either LexError [Token]
tokenize cs = runExcept $ flip evalStateT (LexState startPosition cs) $ tokenize'
  where
    tokenize' = do
      c <- nextChar
      if c == '\0'
        then return []
        else do
          t  <- lexToken
          ts <- tokenize'
          return (t : ts)

-- Characters classes

symbolChar :: [Char]
symbolChar = "!#$%&*+./<=>?@\\^|-~"

-- Reserved symbols

symbols :: [(String, Token')]
symbols = 
  [ ("..", TkDoubleDot)
  , (":", TkColon)
  , ("::", TkDoubleColon)
  , ("=", TkEqual)
  , ("\\", TkLambda)
  , ("|", TkPipe)
  , ("<-", TkLArrow)
  , ("->", TkRArrow)
  , ("@", TkAt)
  , ("~", TkTilde)
  , ("=>", TkFatArrow)
  ]

-- Reserved keywords

keywords :: [(String, Token')]
keywords =
  [ ("case", TkCase)
  , ("class", TkClass)
  , ("data", TkData)
  , ("default", TkDefault)
  , ("deriving", TkDeriving)
  , ("do", TkDo)
  , ("else", TkElse)
 
  , ("if", TkIf)
  , ("import", TkImport)
  , ("in", TkIn)
  , ("infix", TkInfix)
  , ("infixl", TkInfixl)
  , ("infixr", TkInfixr)
  , ("instance", TkInstance)
 
  , ("let", TkLet)
  , ("module", TkModule)
  , ("newtype", TkNewtype)
  , ("of", TkOf)
  , ("then", TkThen)
  , ("type", TkType)
  , ("where", TkWhere)
  , ("_", TkUnderscore)
  ]

lexToken :: Lexer Token
lexToken = do
  c <- nextChar
  if isSpace c
    then consumeChar >> lexToken
    else do
      p <- currentPosition
      tok <- case c of
        -- EOF
        '\0'                    -> return TkEOF
        -- Literals
        '\"'                    -> TkString <$> lexString
        '\''                    -> TkChar <$> lexChar
        _ | isDigit c           -> TkInteger <$> lexInteger
        -- 
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
        _ | c `elem` symbolChar -> lexSymbol
        _ | isAlpha c           -> lexIdentifier
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
    str@(c:_) <- lexIdentifier'
    case lookup str keywords of
      Just t  -> return t
      Nothing | isLower c -> return $ TkIdentifier [] str
      Nothing | isUpper c -> do
        c <- nextChar
        if c /= '.'
          then return $ TkIdentifier [] str
          else do
            TkIdentifier qs n <- consumeChar >> lexIdentifier
            return $ TkIdentifier (str : qs) n
  where
    lexIdentifier' :: Lexer String
    lexIdentifier' = do
      c <- nextChar
      if isAlphaNum c || c `elem` "_'"
        then do
          consumeChar
          (c :) <$> lexIdentifier'
        else return ""

lexSymbol :: Lexer Token'
lexSymbol = do
    p         <- currentPosition
    str@(c:_) <- lexSymbol'
    case lookup str symbols of
      Nothing | str == "--" -> (delocate <$>) $ lexSingleLineComment >> lexToken
      Nothing               -> return $ TkIdentifier []  str
      Just tok              -> return tok
  where
    lexSymbol' = do
      c <- nextChar
      if c `elem` symbolChar
        then consumeChar >> (c :) <$> lexSymbol'
        else return ""

lexInteger :: Lexer Integer
lexInteger = lexInteger' 0
  where
    lexInteger' :: Integer -> Lexer Integer
    lexInteger' i = do
      c <- nextChar
      if isDigit c
        then consumeChar >> lexInteger' (10 * i + fromIntegral (ord c - ord '0'))
        else return i

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