module Syntax.Small.Token where

import Syntax.Location

data Token' = TkIdentifier0 String
            | TkIdentifier1 String
            -- Literals
            | TkInteger Int
            | TkChar Char
            | TkString String
            | TkTrue
            | TkFalse
            -- Keywords
            | TkIf
            | TkThen
            | TkElse
            | TkLet
            | TkIn
            | TkCase
            | TkOf
            | TkDo
            -- Symbols
            | TkArrow
            | TkOr
            | TkAnd
            | TkLT
            | TkLE
            | TkGT
            | TkGE
            | TkEQ
            | TkNE
            | TkColon
            | TkPlus
            | TkMinus
            | TkTimes
            | TkDivides
            -- 
            | TkLParen
            | TkRParen
            | TkLBrace
            | TkRBrace
            | TkLBracket
            | TkRBracket
            | TkComma
            | TkSemiColon
            | TkLambda
            | TkDefine
            -- EOF
            | TkEOF
            deriving (Eq)

instance Show Token' where
  show (TkIdentifier0 id) = id ++ "₀"
  show (TkIdentifier1 id) = id ++ "₁"
  -- Literals
  show (TkInteger i)      = show i
  show (TkChar c)         = show c
  show (TkString s)       = show s
  show TkTrue             = "True"
  show TkFalse            = "False"
  -- Keywords
  show TkIf               = "\'if\'"
  show TkThen             = "\'then\'"
  show TkElse             = "\'else\'"
  show TkLet              = "\'let\'"
  show TkIn               = "\'in\'"
  show TkCase             = "\'case\'"
  show TkOf               = "\'of\'"
  show TkDo               = "\'do\'"
  -- Symbols
  show TkArrow            = "\'->\'"
  show TkOr               = "\'||\'"
  show TkAnd              = "\'&&\'"
  show TkLT               = "\'<\'"
  show TkLE               = "\'<=\'"
  show TkGT               = "\'>\'"
  show TkGE               = "\'>=\'"
  show TkEQ               = "\'==\'"
  show TkNE               = "\'/=\'"
  show TkColon            = "\':\'"
  show TkPlus             = "\'+\'"
  show TkMinus            = "\'-\'"
  show TkTimes            = "\'*\'"
  show TkDivides          = "\'/\'"
  -- 
  show TkLParen           = "\'(\'"
  show TkRParen           = "\')\'"
  show TkLBrace           = "\'{\'"
  show TkRBrace           = "\'}\'"
  show TkLBracket         = "\'[\'"
  show TkRBracket         = "\']\'"
  show TkComma            = "\',\'"
  show TkSemiColon        = "\';\'"
  show TkLambda           = "\'\\\'"
  show TkDefine           = "\'=\'"
  -- EOF
  show TkEOF              = "<EOF>"

type Token = Locate Token'
