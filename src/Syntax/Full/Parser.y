{
module Syntax.Full.Parser where

import Syntax.Full.Token
}

%name parse
%tokentype { Token }
%error { parseError }
%token 
  varId              { t | isVariableIdentifier t }
  conId              { t | isConstructorIdentifier t }
  symId              { t | isSymbolIdentifier t }
  conSymId           { t | isConstructorSymbolIdentifier t }

  integer            { TkInteger _ }
  char               { TkChar _ }
  string             { TkString _ }


  'case'             { TkCase }
  'class'            { TkClass }
  'data'             { TkData }
  'default'          { TkDefault }
  'deriving'         { TkDeriving }
  'do'               { TkDo }
  'else'             { TkElse }

  'if'               { TkIf }
  'import'           { TkImport }
  'in'               { TkIn }
  'infix'            { TkInfix }
  'infixl'           { TkInfixl }
  'infixr'           { TkInfixr }
  'instance'         { TkInstance }

  'let'              { TkLet }
  'module'           { TkModule }
  'newtype'          { TkNewtype }
  'of'               { TkOf }
  'then'             { TkThen }
  'type'             { TkType }
  'where'            { TkWhere }
  '_'                { TkUnderscore }

  '..'               { TkDoubleDot }
  ':'                { TkColon }
  '::'               { TkDoubleColon }
  '='                { TkEqual }
  '\\'               { TkLambda }
  '|'                { TkPipe }
  '<-'               { TkLArrow }
  '->'               { TkRArrow }
  '@'                { TkAt }
  '~'                { TkTilde }
  '=>'               { TkFatArrow }

  '('                { TkLParen }
  ')'                { TkRParen }
  ','                { TkComma }
  ';'                { TkSemiColon }
  '['                { TkLBracket }
  ']'                { TkRBracket }
  '`'                { TkBackTick }
  '{'                { TkLBrace }
  '}'                { TkRBrace }

%%

--

option(X) : X { Just $1 }
          |   { Nothing }

list(X) : list(X) X { $2 : $1 }
        |           { [] }

nonempty_list(X) : list(X) X { $2 : $1 }

separated_list(X, Y) : separated_list(X, Y) X Y { $3 : $1 }
                     | X                        { [$1] }
                     |                          { [] }

separated_nonempty_list(X, Y) : separated_nonempty_list(X, Y) Y X { $3 : $1 }
                              | X                                 { [$1] }

--

module : moduleHeader moduleBody { 0 }

moduleHeader : 'module' moduleName 'where' { 0 }
             |                             { 0 }

moduleBody : '{' separated_nonempty_list(importDeclaration, nonempty_list(';')) '}'                                         { 0 }
           | '{' separated_nonempty_list(importDeclaration, nonempty_list(';')) separated_nonempty_list(topDeclaration, nonempty_list(';')) '}' { 0 }
           | '{' separated_nonempty_list(topDeclaration, nonempty_list(';')) '}'                                            { 0 }

moduleName : conId { 0 }

importDeclaration : 'import' moduleName { 0 }

topDeclaration : 'type' { 0 }
               | 'data' { 0 }
               | 'newtype' { 0 }
               | 'class' { 0 }
               | 'instance' { 0 }
               | 'default' { 0 }
               | declaration { 0 }

declaration : genDeclaration             { 0 }
--            | leftHandSide rightHandSide { 0 }
--            | pattern rightHandSide      { 0 }

genDeclaration : fixity option(integer) symId { 0 }
               | separated_nonempty_list(varId, ',') '::' typeSignature { 0 }

fixity : 'infixl' { 0 }
       | 'infixr' { 0 }
       | 'infix'  { 0 }

typeSignature : 'type' { 0 }



{
}