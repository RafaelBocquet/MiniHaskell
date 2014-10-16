{
module Syntax.Full.Lexer where

import Syntax.Full.Token
}

%wrapper "posn"

-- Character classes

$newline = [\n\r\f]

$digit = [0-9]
$bit   = [0-1]
$hexit = [0-9a-fA-F]
$octit = [0-7]

$symbolChar     = [!#\$\%&\*\+\.\/\<=>\?@\\\^\|\-\~:]
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

-- Reserved symbols
".."       { const (const TkDoubleDot) }
"::"       { const (const TkDoubleColon) }
"="        { const (const TkEqual) }
"\\"       { const (const TkLambda) }
"|"        { const (const TkPipe) }
"<-"       { const (const TkLArrow) }
"->"       { const (const TkRArrow) }
"@"        { const (const TkAt) }
"~"        { const (const TkTilde) }
"=>"       { const (const TkFatArrow) }

-- Reserved keywords

"case"     { const (const TkCase) }
"class"    { const (const TkClass) }
"data"     { const (const TkData) }
"default"  { const (const TkDefault) }
"deriving" { const (const TkDeriving) }
"do"       { const (const TkDo) }
"else"     { const (const TkElse) }
 
"if"       { const (const TkIf) }
"import"   { const (const TkImport) }
"in"       { const (const TkIn) }
"infix"    { const (const TkInfix) }
"infixl"   { const (const TkInfixl) }
"infixr"   { const (const TkInfixr) }
"instance" { const (const TkInstance) }
 
"let"      { const (const TkLet) }
"module"   { const (const TkModule) }
"newtype"  { const (const TkNewtype) }
"of"       { const (const TkOf) }
"then"     { const (const TkThen) }
"type"     { const (const TkType) }
"where"    { const (const TkWhere) }
"_"        { const (const TkUnderscore) }

\(          { const (const TkLParen) }
\)          { const (const TkRParen) }
\,          { const (const TkComma) }
\;          { const (const TkSemiColon) }
\[          { const (const TkLBracket) }
\]          { const (const TkRBracket) }
`           { const (const TkBackTick) }
\{          { const (const TkLBrace) }
\}          { const (const TkRBrace) }

@moduleName @varId { const (TkIdentifier []) }
@moduleName @conId { const (TkIdentifier []) }
@moduleName @symId { const (TkIdentifier []) }

@integer    { const (TkInteger . read) }

{

ar = alexScanTokens "ok"
tokenise = alexScanTokens

}
