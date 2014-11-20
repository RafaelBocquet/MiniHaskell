{
module Syntax.Full.Lexer where

import Syntax.Full.Token
import Syntax.Full.Layout
import Syntax.Location

import Data.Char (isAlpha)

}

%wrapper "monad"

-- Character classes

$newline = [\n\r\f]

$digit = [0-9]
$bit   = [0-1]
$hexit = [0-9a-fA-F]
$octit = [0-7]

$symbolChar     = [!\#\$\%&\*\+\.\/\<=>\?@\\\^\|\-\~:]
$smallChar      = [a-z]
$largeChar      = [A-Z]
$identifierChar = [a-zA-Z0-9_\']

@varId      = $smallChar $identifierChar* 
@conId      = $largeChar $identifierChar* 
@symId      = $symbolChar+
@moduleName = (@conId \.)*

@integer = $digit+

:-

($white | $newline)+;

\-\-(. # $symbolChar)(. # $newline)*$newline;

-- Reserved symbols
".."       { alexSimpleToken TkDoubleDot }
"::"       { alexSimpleToken TkDoubleColon }
"="        { alexSimpleToken TkEqual }
\\         { alexSimpleToken TkLambda }
"|"        { alexSimpleToken TkPipe }
"<-"       { alexSimpleToken TkLArrow }
"->"       { alexSimpleToken TkRArrow }
"@"        { alexSimpleToken TkAt }
"~"        { alexSimpleToken TkTilde }
"=>"       { alexSimpleToken TkFatArrow }

-- Reserved keywords

"case"     { alexSimpleToken TkCase }
"class"    { alexSimpleToken TkClass }
"data"     { alexSimpleToken TkData }
"default"  { alexSimpleToken TkDefault }
"deriving" { alexSimpleToken TkDeriving }
"do"       { alexSimpleToken TkDo }
"else"     { alexSimpleToken TkElse }
 
"if"       { alexSimpleToken TkIf }
"import"   { alexSimpleToken TkImport }
"in"       { alexSimpleToken TkIn }
"infix"    { alexSimpleToken TkInfix }
"infixl"   { alexSimpleToken TkInfixl }
"infixr"   { alexSimpleToken TkInfixr }
"instance" { alexSimpleToken TkInstance }
 
"let"      { alexSimpleToken TkLet }
"module"   { alexSimpleToken TkModule }
"newtype"  { alexSimpleToken TkNewtype }
"of"       { alexSimpleToken TkOf }
"then"     { alexSimpleToken TkThen }
"type"     { alexSimpleToken TkType }
"where"    { alexSimpleToken TkWhere }
"_"        { alexSimpleToken TkUnderscore }

\(          { alexSimpleToken TkLParen }
\)          { alexSimpleToken TkRParen }
\,          { alexSimpleToken TkComma }
\;          { alexSimpleToken TkSemiColon }
\[          { alexSimpleToken TkLBracket }
\]          { alexSimpleToken TkRBracket }
`           { alexSimpleToken TkBackTick }
\{          { alexSimpleToken TkLBrace }
\}          { alexSimpleToken TkRBrace }

@moduleName @varId { alexToken (\(splitModuleName -> s) -> TkIdentifier (init s) (last s)) }
@moduleName @conId { alexToken (\(splitModuleName -> s) -> TkIdentifier (init s) (last s)) }
@moduleName @symId { alexToken (\(splitModuleName -> s) -> TkIdentifier (init s) (last s)) }

@integer    { alexToken (const (TkInteger 0)) }
\'a\'       { alexSimpleToken (TkChar 'a') }

{

data AlexUserState = AlexUserState

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState

alexEOF = do
  (AlexPn a r c, _, _, _) <- alexGetInput
  return (Token TkEOF (Location (Position a r c) 0) c)

splitModuleName :: String -> [String]
splitModuleName = uncurry (:) . splitModuleName'
  where
    splitModuleName' "."                          = (".", [])
    splitModuleName' ('.':xs) | isAlpha (head xs) = ([], splitModuleName xs)
                              | otherwise         = (':':xs, [])
    splitModuleName' (x:xs)                       = let (ys, zs) = splitModuleName' xs in (x:ys, zs)
    splitModuleName' []                           = ([], [])

alexToken :: (String -> Token') -> AlexAction Token
alexToken t (AlexPn a r c, _, _, s) l  = do
  return (Token (t $ take l s) (Location (Position a r c) l) c)

alexSimpleToken :: Token' -> AlexAction Token
alexSimpleToken = alexToken . const

alexScanTokens :: Alex [Token]
alexScanTokens = do
  tk <- alexMonadScan
  case tk of
   t@(tokenToken -> TkEOF) ->
     return [t]
   _ -> do
     tks <- alexScanTokens
     return (tk : tks)

tokenise :: String -> Either String [Token]
tokenise s = makeLayout `fmap` runAlex s alexScanTokens

}
