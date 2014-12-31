{-# LANGUAGE LambdaCase, ViewPatterns, PatternSynonyms, TupleSections #-}

module Parsing.Token where

import Parsing.Location

import Data.Char

data Token' = TkIdentifier [String] String
            -- Literals
            | TkInteger Int
            | TkChar Char
            | TkString String
            -- Keywords
            | TkCase
            | TkClass
            | TkData
            | TkDefault
            | TkDeriving
            | TkDo
            | TkElse
 
            | TkIf
            | TkImport
            | TkIn
            | TkInfix
            | TkInfixl
            | TkInfixr
            | TkInstance
 
            | TkLet
            | TkModule
            | TkNewtype
            | TkOf
            | TkThen
            | TkType
            | TkWhere
            | TkUnderscore
            -- Symbols
            | TkDoubleDot
            | TkColon
            | TkDoubleColon
            | TkEqual
            | TkLambda
            | TkPipe
            | TkLArrow
            | TkRArrow
            | TkAt
            | TkTilde
            | TkFatArrow
            -- Special
            | TkLParen
            | TkRParen
            | TkComma
            | TkSemiColon
            | TkLBracket
            | TkRBracket
            | TkBackTick
            | TkLBrace
            | TkRBrace
            -- EOF
            | TkEOF
            deriving (Eq, Show)

data Token = Token
             { tokenToken :: Token'
             , tokenLocation :: Location
             , tokenIndentation :: Int
             }

instance Show Token where
  show (tokenToken -> t) = show t

isVariableIdentifier :: Token -> Bool
isVariableIdentifier (tokenToken -> TkIdentifier [] (c:_)) | isLower c = True
isVariableIdentifier _                                                = False

isConstructorIdentifier :: Token -> Bool
isConstructorIdentifier (tokenToken -> TkIdentifier [] (c:_)) | isUpper c = True
isConstructorIdentifier _                                                = False

isSymbolIdentifier :: Token -> Bool
isSymbolIdentifier (tokenToken -> TkIdentifier [] (c:_)) | not (isAlpha c) && c /= ':' = True
isSymbolIdentifier _                                                              = False

isSymbolConstructorIdentifier :: Token -> Bool
isSymbolConstructorIdentifier (tokenToken -> TkIdentifier [] (':':_)) = True
isSymbolConstructorIdentifier _                                      = False

isQVariableIdentifier :: Token -> Bool
isQVariableIdentifier (tokenToken -> TkIdentifier [] (c:_)) | isLower c = False
isQVariableIdentifier (tokenToken -> TkIdentifier _ (c:_))  | isLower c = True
isQVariableIdentifier _                                                = False

isQConstructorIdentifier :: Token -> Bool
isQConstructorIdentifier (tokenToken -> TkIdentifier [] (c:_)) | isUpper c = False
isQConstructorIdentifier (tokenToken -> TkIdentifier _ (c:_))  | isUpper c = True
isQConstructorIdentifier _                                                = False

isQSymbolIdentifier :: Token -> Bool
isQSymbolIdentifier (tokenToken -> TkIdentifier [] (c:_)) | not (isAlpha c) && c /= ':' = False
isQSymbolIdentifier (tokenToken -> TkIdentifier _ (c:_))  | not (isAlpha c) && c /= ':' = True
isQSymbolIdentifier _                                                              = False

isQSymbolConstructorIdentifier :: Token -> Bool
isQSymbolConstructorIdentifier (tokenToken -> TkIdentifier [] (':':_)) = False
isQSymbolConstructorIdentifier (tokenToken -> TkIdentifier _ (':':_))  = True
isQSymbolConstructorIdentifier _                                      = False

identifierName :: Token -> ([String], String)
identifierName (tokenToken -> TkIdentifier a b) = (a, b)

integerValue :: Token -> Int
integerValue (tokenToken -> TkInteger i) = i

charValue :: Token -> Char
charValue (tokenToken -> TkChar c) = c

stringValue :: Token -> String
stringValue (tokenToken -> TkString s) = s

isFixity :: Token -> Bool
isFixity (tokenToken -> TkInfix)  = True
isFixity (tokenToken -> TkInfixr) = True
isFixity (tokenToken -> TkInfixl) = True
isFixity _                        = False
