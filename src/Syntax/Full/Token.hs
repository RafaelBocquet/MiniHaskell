module Syntax.Full.Token where

import Syntax.Location

import Data.Char

data Token' = TkIdentifier [String] String
            -- Literals
            | TkInteger Integer
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
isVariableIdentifier (TkIdentifier _ (c:_)) | isLower c = True
isVariableIdentifier _                                  = False

isConstructorIdentifier :: Token' -> Bool
isConstructorIdentifier (TkIdentifier _ (c:_)) | isUpper c = True
isConstructorIdentifier _                                  = False

isSymbolIdentifier :: Token' -> Bool
isSymbolIdentifier (TkIdentifier _ (c:_)) | not (isAlpha c) && c /= ':' = True
isSymbolIdentifier _                                                    = False

isSymbolConstructorIdentifier :: Token' -> Bool
isSymbolConstructorIdentifier (TkIdentifier _ (':':_)) = True
isSymbolConstructorIdentifier _                        = False

isFixity :: Token' -> Bool
isFixity TkInfix  = True
isFixity TkInfixr = True
isFixity TkInfixl = True
isFixity _        = False