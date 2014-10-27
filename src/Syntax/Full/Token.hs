module Syntax.Full.Token where

import Syntax.Location

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

type Token = Locate Token'

isVariableIdentifier :: Token' -> Bool
isVariableIdentifier (TkIdentifier [] (c:_)) | isLower c = True
isVariableIdentifier _                                  = False

isConstructorIdentifier :: Token' -> Bool
isConstructorIdentifier (TkIdentifier [] (c:_)) | isUpper c = True
isConstructorIdentifier _                                  = False

isSymbolIdentifier :: Token' -> Bool
isSymbolIdentifier (TkIdentifier [] (c:_)) | not (isAlpha c) && c /= ':' = True
isSymbolIdentifier _                                                    = False

isSymbolConstructorIdentifier :: Token' -> Bool
isSymbolConstructorIdentifier (TkIdentifier [] (':':_)) = True
isSymbolConstructorIdentifier _                        = False

isQVariableIdentifier :: Token' -> Bool
isQVariableIdentifier (TkIdentifier [] (c:_)) | isLower c = False
isQVariableIdentifier (TkIdentifier _ (c:_)) | isLower c = True
isQVariableIdentifier _                                  = False

isQConstructorIdentifier :: Token' -> Bool
isQConstructorIdentifier (TkIdentifier [] (c:_)) | isUpper c = False
isQConstructorIdentifier (TkIdentifier _ (c:_)) | isUpper c = True
isQConstructorIdentifier _                                  = False

isQSymbolIdentifier :: Token' -> Bool
isQSymbolIdentifier (TkIdentifier [] (c:_)) | not (isAlpha c) && c /= ':' = False
isQSymbolIdentifier (TkIdentifier _ (c:_)) | not (isAlpha c) && c /= ':' = True
isQSymbolIdentifier _                                                    = False

isQSymbolConstructorIdentifier :: Token' -> Bool
isQSymbolConstructorIdentifier (TkIdentifier [] (':':_)) = False
isQSymbolConstructorIdentifier (TkIdentifier _ (':':_)) = True
isQSymbolConstructorIdentifier _                        = False

identifierName :: Token' -> ([String], String)
identifierName (TkIdentifier a b) = (a, b)

integerValue :: Token' -> Int
integerValue (TkInteger i) = i

charValue :: Token' -> Char
charValue (TkChar c) = c

stringValue :: Token' -> String
stringValue (TkString s) = s

isFixity :: Token' -> Bool
isFixity TkInfix  = True
isFixity TkInfixr = True
isFixity TkInfixl = True
isFixity _        = False
