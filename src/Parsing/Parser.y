{
{-# LANGUAGE ViewPatterns #-}

module Parsing.Parser where

import Annotation
import Syntactic

import Parsing.Token
import Parsing.Bindings
import Parsing.ModuleBody
import Parsing.Monad
import Parsing.Util

import Syntax.Expression
import Syntax.Type
import Syntax.Name
import Syntax.Module
import Parsing.Location

import Control.Applicative
import Control.Monad
import Control.Lens

import Data.List
import Data.Maybe
import Data.Monoid
import Data.Foldable

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map

import Control.Monad.Except
import Control.Monad.State
}

%name parseModule module
%tokentype { Token }

%monad { ParseMonad }
%error { parseError }

%token 
  tvarId      { t | isVariableIdentifier t }
  tconId      { t | isConstructorIdentifier t }
  tsymId      { t | isSymbolIdentifier t }
  tsymconId   { t | isSymbolConstructorIdentifier t }

  tqvarId     { t | isQVariableIdentifier t }
  tqconId     { t | isQConstructorIdentifier t }
  tqsymId     { t | isQSymbolIdentifier t }
  tqsymconId  { t | isQSymbolConstructorIdentifier t }

  integer     { (tokenToken -> TkInteger _) }
  char        { (tokenToken -> TkChar _) }
  string      { (tokenToken -> TkString _) }


  'case'      { (tokenToken -> TkCase) }
  'class'     { (tokenToken -> TkClass) }
  'data'      { (tokenToken -> TkData) }
  'default'   { (tokenToken -> TkDefault) }
  'deriving'  { (tokenToken -> TkDeriving) }
  'do'        { (tokenToken -> TkDo) }
  'else'      { (tokenToken -> TkElse) }

  'if'        { (tokenToken -> TkIf) }
  'import'    { (tokenToken -> TkImport) }
  'in'        { (tokenToken -> TkIn) }
  'infix'     { (tokenToken -> TkInfix) }
  'infixl'    { (tokenToken -> TkInfixl) }
  'infixr'    { (tokenToken -> TkInfixr) }
  'instance'  { (tokenToken -> TkInstance) }

  'let'       { (tokenToken -> TkLet) }
  'module'    { (tokenToken -> TkModule) }
  'newtype'   { (tokenToken -> TkNewtype) }
  'of'        { (tokenToken -> TkOf) }
  'then'      { (tokenToken -> TkThen) }
  'type'      { (tokenToken -> TkType) }
  'where'     { (tokenToken -> TkWhere) }
  '_'         { (tokenToken -> TkUnderscore) }

  'qualified' { (tokenToken -> TkQualified) }
  'as'        { (tokenToken -> TkAs) }
  'hiding'    { (tokenToken -> TkHiding) }

  '..'        { (tokenToken -> TkDoubleDot) }
  ':'         { (tokenToken -> TkColon) }
  '::'        { (tokenToken -> TkDoubleColon) }
  '='         { (tokenToken -> TkEqual) }
  '\\'        { (tokenToken -> TkLambda) }
  '|'         { (tokenToken -> TkPipe) }
  '<-'        { (tokenToken -> TkLArrow) }
  '->'        { (tokenToken -> TkRArrow) }
  '@'         { (tokenToken -> TkAt) }
  '~'         { (tokenToken -> TkTilde) }
  '=>'        { (tokenToken -> TkFatArrow) }

  '('         { (tokenToken -> TkLParen) }
  ')'         { (tokenToken -> TkRParen) }
  ','         { (tokenToken -> TkComma) }
  ';'         { (tokenToken -> TkSemiColon) }
  '['         { (tokenToken -> TkLBracket) }
  ']'         { (tokenToken -> TkRBracket) }
  '`'         { (tokenToken -> TkBackTick) }
  '{'         { (tokenToken -> TkLBrace) }
  '}'         { (tokenToken -> TkRBrace) }

  EOF         { (tokenToken -> TkEOF) }

%nonassoc 'if' 'then' 'else' '\\' '->' 'let' 'in'
%nonassoc tvarId tconId tsymId tsymconId tqvarId tqconId tqsymId tqsymconId '`'

%nonassoc LOW
%nonassoc MID
%nonassoc HIGH

%%

--

option(X) : X { Just $1 }
          |   { Nothing }

list_L(X) : list_L(X) X { $2 : $1 }
          |           { [] }
list(X) : list_L(X) { reverse $1 }

list_(X) : X list_(X) { $1 : $2 }
         |            { [] }

nonempty_list_L(X) : list_L(X) X { $2 : $1 }
nonempty_list(X) : nonempty_list_L(X) { reverse $1 }

nonempty_list_(X) : X list_(X) { $1 : $2 }

separated_list_L(X, Y) : separated_list_L(X, Y) Y X { $3 : $1 }
                       | X                          { [$1] }
                       |                            { [] }
separated_list(X, Y) : separated_list_L(X, Y) { reverse $1 }

separated_nonempty_list(X, Y) : separated_nonempty_list(X, Y) Y X { $1 ++ [$3] }
                              | X                                 { [$1] }

separated_nonempty_list_(X, Y) : X Y separated_nonempty_list_(X, Y) { $1 : $3 }
                               | X                                  { $1 }

delimited(A, X, B) : A X B { $2 }
preceded(A, X) : A X { $2 }
terminated(X, B) : X B { $1 }

--

varId    : tvarId      { Name NsVar localName (snd $ identifierName $1) }
         | 'qualified' { Name NsVar localName "qualified" }
         | 'as'        { Name NsVar localName "as" }
         | 'hiding'    { Name NsVar localName "hiding" }
conId    : tconId      { Name NsCon localName (snd $ identifierName $1) }
symId    : tsymId      { Name NsVar localName (snd $ identifierName $1) }
symconId : tsymconId   { Name NsCon localName (snd $ identifierName $1) }

tyvarId    : tvarId      { Name NsTyVar localName (snd $ identifierName $1) }
           | 'qualified' { Name NsTyVar localName "qualified" }
           | 'as'        { Name NsTyVar localName "as" }
           | 'hiding'    { Name NsTyVar localName "hiding" }
tyconId    : tconId      { Name NsTyCon localName (snd $ identifierName $1) }
tysymId    : tsymId      { Name NsTyVar localName (snd $ identifierName $1) }
tysymconId : tsymconId   { Name NsTyCon localName (snd $ identifierName $1) }

qvarId    : tvarId     { Name  NsVar   localName                              (snd $ identifierName $1) }
          | tqvarId    { Name  NsVar   (ModuleName $ fst $ identifierName $1) (snd $ identifierName $1) }
qconId    : tconId     { Name  NsCon   localName                              (snd $ identifierName $1) }
          | tqconId    { Name  NsCon   (ModuleName $ fst $ identifierName $1) (snd $ identifierName $1) }
qsymId    : tsymId     { Name  NsVar   localName                              (snd $ identifierName $1) }
          | tqsymId    { Name  NsVar   (ModuleName $ fst $ identifierName $1) (snd $ identifierName $1) }
qsymconId : tsymconId  { Name  NsCon   localName                              (snd $ identifierName $1) }
          | tqsymconId { Name  NsCon   (ModuleName $ fst $ identifierName $1) (snd $ identifierName $1) }

qtyvarId    : tvarId     { Name NsTyVar localName                              (snd $ identifierName $1) }
            | tqvarId    { Name NsTyVar (ModuleName $ fst $ identifierName $1) (snd $ identifierName $1) }
qtyconId    : tconId     { Name NsTyCon localName                              (snd $ identifierName $1) }
            | tqconId    { Name NsTyCon (ModuleName $ fst $ identifierName $1) (snd $ identifierName $1) }
qtysymId    : tsymId     { Name NsTyVar localName                              (snd $ identifierName $1) }
            | tqsymId    { Name NsTyVar (ModuleName $ fst $ identifierName $1) (snd $ identifierName $1) }
qtysymconId : tsymconId  { Name NsTyCon localName                              (snd $ identifierName $1) }
            | tqsymconId { Name NsTyCon (ModuleName $ fst $ identifierName $1) (snd $ identifierName $1) }

svarId : varId { $1 }
       | delimited('(', symId, ')') { $1 }
sconId : conId { $1 }
       | delimited('(', symconId, ')') { $1 }
       | delimited('(', nonempty_list(','), ')') { tupleName (length $1 + 1) }
qsvarId : qvarId { $1 }
        | delimited('(', qsymId, ')') { $1 }
qsconId : qconId { $1 }
        | delimited('(', qsymconId, ')') { $1 }
--        | delimited('(', nonempty_list(','), ')') { tupleName (length $1 + 1) }

styvarId : tyvarId { $1 }
         | delimited('(', tysymId, ')') { $1 }
styconId : tyconId { $1 }
         | delimited('(', tysymconId, ')') { $1 }
         | delimited('(', nonempty_list(','), ')') { tupleTypeName (length $1 + 1) }
qstyvarId : qtyvarId { $1 }
          | delimited('(', qtysymId, ')') { $1 }
qstyconId : qtyconId { $1 }
          | delimited('(', qtysymconId, ')') { $1 }
--          | delimited('(', nonempty_list(','), ')') { tupleTypeName (length $1 + 1) }
-- 

module : moduleHeader moduleBody EOF {% do
                                          let ModuleBody imps decls tdecls cdecls idecls = $2   
                                          decls' <- makeDeclarationMap =<< decls emptyBindings
                                          return $ Module (fst $1) (snd $1) imps decls' tdecls cdecls idecls
                                     }

moduleHeader : 'module' moduleName option(delimited('(', separated_list(export, ','), ')')) 'where'
                 { ($2, $3) }
             |   { (ModuleName ["Main"], Nothing) }

moduleBody :: { ModuleBody }
           : '{' list(';') separated_nonempty_list(importDeclaration, nonempty_list(';')) nonempty_list(';') separated_nonempty_list(topDeclaration, nonempty_list(';')) list(';') '}'
                  {% concatBody (moduleBodyImport $3 : $5) }
           | '{' list(';') separated_nonempty_list(topDeclaration, nonempty_list(';')) list(';') '}'
                  {% concatBody $3 }

moduleName :: { ModuleName }
           : qconId { let Name _ a b = $1 in a <> ModuleName [b] }

importDeclaration :: { ModuleImport Name }
                  : 'import' option('qualified') moduleName option(preceded('as', moduleName)) option(delimited('(', separated_list(import, ','), ')')) option(preceded('hiding', delimited('(', separated_list(import, ','), ')')))
                     { ModuleImport (isJust $2) $3 (maybe $3 id $4) $5 (maybe [] id $6) }

import :: { ModuleImportSpec Name }
import : varId
           { InpVar $1 }
       | tyconId '(' '..' ')'
           { InpAll $1 }
       | tyconId '(' separated_list(conId, ',') ')'
           { InpFilter $1 $3 }

export :: { ModuleExportSpec Name } 
export : varId
           { ExpVar $1 }
       | tyconId '(' '..' ')'
           { ExpAll $1 }
       | tyconId '(' separated_list(conId, ',') ')'
           { ExpFilter $1 $3 }
       | 'module' moduleName
           { ExpModule $2 }

topDeclaration :: { ModuleBody }
               : 'type' simpletype '=' type
                   { moduleBodyTypeDeclaration (fst $2) (TypeDeclaration (snd $2) $4) }
               | 'data' {- context -} simpletype '=' separated_nonempty_list(constructor, '|')
                   { moduleBodyTypeDeclaration (fst $2) (DataDeclaration (snd $2) $4) }
--                -- | 'newtype' context simpletype '=' conId list(atype) { 0 }
--               | 'class' {- simplecontext -} conId varId option(preceded('where', delimited('{', separated_list(classDeclaration, nonempty_list(';')), '}')))
--                | 'instance' {- simplecontext -} qtyconId instance option(preceded('where', delimited('{', separated_list(instanceDeclaration, nonempty_list(';')), '}')))
--                     {% do
--                       decls <- makeDeclarationMap $ maybe Map.empty (foldr addBinding Map.empty) $4
--                       return [TopInstanceDeclaration $2 (fst $3) (InstanceDeclaration (snd $3) decls)]
--                     }
--             -- | 'default' { 0 }
               | declaration { moduleBodyBindingDeclaration $1 }
--               | fixity option(integer) qsvarId { 0 }
--               | fixity option(integer) qsconId { 0 }


declaration :: { Bindings -> ParseMonad Bindings }
            : separated_nonempty_list(svarId, ',') '::' typeSignature
                { foldrM (\x -> addSignature x $3) ?? $1 }
            | svarId nonempty_list(apattern) rightHandSide
                { addDeclaration $1 $2 $3 }
            | pattern rightHandSide
                { addPattern $1 $2 }

fixity :: { Fixity }
       : 'infixl' { Infixl }
       | 'infixr' { Infixr }
       | 'infix'  { Infix }

-- TODO : guards
rightHandSide :: { Expr Name () } 
              : '=' expression                { $2 }

constructor :: { DataConstructor Name }
            : sconId list(atype)    { DataConstructor $1 $2 }
-- shift reduce conflict here
--            | btype symconId btype  { DataConstructor $2 [$1, $3] }

-- ---- Types

typeSignature : type { $1 }
          --  | context conId list(varId) { 0 }

simpletype :: { (Name, [Name]) }
           : styconId list(tyvarId) { ($1, $2) }

type :: { Type Name () }
     : separated_nonempty_list(btype, '->')
        { foldr arrowType (last $1) (init $1) }

btype :: { Type Name () }
      : nonempty_list(atype) { makeTypeApplication (head $1) (tail $1) }

atype :: { Type Name () }
      : gtycon            { $1 }
      | tyvarId           { tyVar $1 }
      | delimited('(', separated_nonempty_list(type, ','), ')')
          { case length $1 of
             1 -> head $1
             _ -> makeTypeApplication
                  (tyCon $ tupleTypeName (length $1))
                  $1
          }
      | delimited('[', type, ']')
          { tyApp (tyCon $ primitiveTyCon "[]") $1 }

gtycon :: { Type Name () }
       : '(' ')'                    { tyCon (primitiveTyCon "()") }
--       | '(' nonempty_list(',') ')' { tyCon (tupleTypeName (length $2)) }
       | '[' ']'                    { tyCon (primitiveTyCon "[]") }
--     | '(' '->' ')'               { tyCon (primitiveTyCon "->") }
       | qstyconId                   { tyCon $1 }

---- Classes, context

-- context : class '=>'                                           { 0 }
--         | delimited('(', separated_list(class, ','), ')') '=>' { 0 }

-- simplecontext : simpleclass '=>'                                           { 0 }
--               | delimited('(', separated_list(simpleclass, ','), ')') '=>' { 0 }

-- class : qconId varId                      { 0 }
--       | qconId delimited('(', atype, ')') { 0 }

-- simpleclass : qconId varId                      { 0 }

-- classDeclaration :: { [ (Name, MonoType Name) ] }
--                  : separated_nonempty_list(svarId, ',') '::' typeSignature { fmap (\x -> (x, $3)) $1 }

-- instance :: { (MonoType Name, [Name]) }
--          : gtycon                                                { ($1, []) }
--          | '(' gtycon list(varId) ')'                            { ($2, $3) }
--          | '(' varId ',' separated_nonempty_list(varId, ',') ')' { (TyConstant (Name ["Primitive"] TypeNsCon (UserName (replicate (length $4) ','))), $2 : $4) }
--          | '[' varId ']'                                         { (TyConstant (Name ["Primitive"] TypeNsCon (UserName "[]")), [$2]) }
--          | '(' varId '->' varId ')'                               { (TyArrow, [$2, $4]) }

-- instanceDeclaration :: { VariableDeclaration }
--                     : leftHandSide rightHandSide { VariableDeclaration (fst $1) (snd $1) $2 }

-- Exprs

operator :: { Expr Name () }
         : qsymId         { eVar $1 }
         | qsymconId      { eVar $1 }
         | '`' qconId '`' { eVar $2 }
         | '`' qvarId '`' { eVar $2 }

expression :: { Expr Name () }
           : expression10                     { $1 }
           | expression operator expression10 { eApp (eApp $2 $1) $3 }

expression10 :: { Expr Name () }
             : 'if' expression 'then' expression 'else' expression
                  { eCase $2
                    [ (PCon (Name NsCon (primitiveModule) "True") [] [], $4)
                    , (PCon (Name NsCon (primitiveModule) "False") [] [], $6)
                    ]
                  }
             | '\\'  nonempty_list(apattern) '->' expression
                  {% makeAbs $2 $4 }
             | 'let' '{' list(';') separated_nonempty_list(declaration, nonempty_list(';')) list(';') '}' 'in' expression
                  {% do
                       decls <- makeDeclarationMap =<< foldlM (&) emptyBindings $4
                       return $ eLet (Map.toList $ decls) $8
                  }
             | 'case' expression 'of' '{' separated_nonempty_list(option(casealternative), ';') '}'
                  { eCase $2 (fmap fromJust . filter isJust $ $5) }
             | 'do' '{' list(';') separated_nonempty_list(doBinding, nonempty_list(';')) list(';') '}'
                  { undefined }
             | nonempty_list(aexpression) { foldl eApp (head $1) (tail $1) }

aexpression :: { Expr Name () }
            : qsvarId { eVar $1 }
            | qsconId { eVar $1 }
            | integer { eInt (integerValue $1) }
            | char    { eChar (charValue $1) }
            | string
                { foldl
                  (\a b -> eApp (eApp (eVar (primitiveCon ":")) a) (eChar b))
                  (eVar (primitiveCon "[]"))
                  (stringValue $1)
                }
            | delimited('(', separated_list(expression, ','), ')')
                { case length $1 of
                    0 -> eVar (primitiveCon "()")
                    1 -> head $1
                    n -> foldl eApp (eVar (tupleName n)) $1
                }
            | delimited('[', separated_list(expression, ','), ']')
                { foldl
                  (\a b -> eApp (eApp (eVar (primitiveCon ":")) a) b)
                  (eVar (primitiveCon "[]"))
                  $1
                }

casealternative : pattern '->' expression { ($1, $3) }
             -- | pattern '|' expression '->' expression { ($1, $5) }

doBinding : expression
              { undefined }
-- require \infty-lookahead to know whether we are parsing a pattern or an expression
-- since pattern < expression, we could parse the pattern as an expression and check it is a valid pattern
-- this is what ghc does
-- or restrict patterns to simple ones
--          | pattern '<-' expression
--              { undefined }
          | 'let' '{' list(';') separated_nonempty_list(declaration, nonempty_list(';')) list(';') '}'
              { undefined }

-- Patterns

pattern :: { Pat Name }
        : pattern10 { $1 }
        | pattern qsymconId pattern10 { PCon $2 [$1, $3] [] }

pattern10 :: { Pat Name }
          : apattern { $1 }
          | qconId nonempty_list_(apattern)
              { PCon $1 $2 [] }

apattern :: { Pat Name }
         : varId                        { PAny [$1] }
         | varId '@' apattern           { $3 & unAnnotate.patternVariables %~ ($1 :) }
         | '_'                          { PAny [] }
         | qconId                       { PCon $1 [] [] }
         | delimited('(', separated_list(pattern, ','), ')')
              { case $1 of
                 []    -> PCon (primitiveCon "()") [] []
                 [pat] -> pat
                 ps    -> PCon (tupleName (length ps)) $1 []
              }
         | delimited('[', separated_list(pattern, ','), ']')
              { foldr
                (\x y -> PCon (primitiveCon ":") [x, y] [])
                (PCon (primitiveCon ":") [] [])
                $1
              }

{

data Fixity = Infixl | Infixr | Infix


-- data VariableDeclaration = VariableDeclaration Name [Pattern Name] (Expr Name ())
--                          | SignatureDeclaration Name (MonoType Name)
--                          | PatternDeclaration (Pattern Name) (Expr Name ())

-- type BindingMap = Map Name (Maybe (MonoType Name), [([Pattern Name], Expr Name ())])

-- addBinding :: VariableDeclaration -> BindingMap -> BindingMap
-- addBinding (VariableDeclaration n p e) m = case Map.lookup n m of
--   Nothing               -> Map.insert n (Nothing, [(p, e)]) m
--   Just (t, [])          -> Map.insert n (t, [(p, e)]) m
--   Just (t, es@((p', e'):_))
--     | length p' == length p -> Map.insert n (t, (p, e):es) m
--     | otherwise -> error "Bad pattern length ..."
-- addBinding (SignatureDeclaration n t) m = case Map.lookup n m of
--   Nothing           -> Map.insert n (Just t, []) m
--   Just (Just _, _)  -> error "Multiple signatures"
--   Just (Nothing, e) -> Map.insert n (Just t, e) m
-- -- addBinding (PatternDeclaration p e) m = case 

-- makeDeclarationMap :: BindingMap -> ParseMonad (DeclarationMap Name)
-- makeDeclarationMap mp =
--     Map.fromList <$> (\(a, b) -> do
--       b' <- makeDeclaration b
--       return (a, b')
--     ) `mapM` Map.toList mp
--   where
--     makeDeclaration (t, e@((pats, _):_)) = do
--       let ne = length pats
--       ns <- forM [1..ne] $ const generateName
--       let decl = foldr
--             (((.).(.)) ann ELambda)
--             (case ne of
--               0 -> snd (head e)
--               1 ->
--                 ann $ ECase
--                 (ann . EVar' . Name [] NsVar $ head ns)
--                 (fmap (\(pat, e) -> (head pat, e)) e)
--               _ ->
--                 ann $ ECase
--                 (makeApplication (ann $ EVar' (Name ["Primitive"] NsCon (replicate (ne - 1) ','))) (ann . EVar' . Name [] NsVar <$> ns))
--                 (fmap (\(pat, e) -> (Pattern (PConstructor (Name ["Primitive"] NsCon (replicate (ne - 1) ',')) pat) [], e)) e)
--             )
--             ns
--       return $ Declaration t decl
-- --    makeDeclaration (t, []) = throwError ParseError

-- data TopDeclaration = ImportDeclaration ModuleName
--                     | TopVariableDeclaration VariableDeclaration
--                     | TopTypeDeclaration Name (TypeDeclaration Name)
--                     | TopClassDeclaration Name (ClassDeclaration Name)
--                     | TopInstanceDeclaration QName (MonoType Name) (InstanceDeclaration Name)

-- makeModule :: ModuleName -> [TopDeclaration] -> ParseMonad (Module Name)
-- makeModule n tds = do
--   let (m, bs) = foldl
--         (flip addTopDeclaration)
--         (Module n Set.empty Map.empty Map.empty Map.empty Map.empty, Map.empty)
--         tds
--   ds <- makeDeclarationMap bs
--   return $ m { moduleDeclarations = ds }
--   where
--     addTopDeclaration (ImportDeclaration impName) (m, bs)    = (m { moduleImport = Set.insert impName (moduleImport m) }, bs)
--     addTopDeclaration (TopVariableDeclaration v) (m, bs)     = (m, addBinding v bs)
--     addTopDeclaration (TopTypeDeclaration a b) (m, bs)       = (m { moduleTypeDeclarations = Map.insert a b (moduleTypeDeclarations m) }, bs)
--     addTopDeclaration (TopClassDeclaration a b) (m, bs)      = (m { moduleClassDeclarations = Map.insert a b (moduleClassDeclarations m) }, bs)
--     addTopDeclaration (TopInstanceDeclaration a b c) (m, bs) = (m { moduleInstanceDeclarations = Map.insert (a, b) c (moduleInstanceDeclarations m) }, bs)

parseError x = error $ "Parse error : " ++ show x

}
