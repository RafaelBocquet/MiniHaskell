{
{-# LANGUAGE ViewPatterns #-}

module Parsing.Parser where

import Annotation

import Parsing.Token
import Syntax.Expression
import Syntax.Type
import Syntax.Name
import Syntax.Module
import Parsing.Location

import Control.Applicative
import Control.Monad
import Control.Lens

import Data.Maybe
import Data.Monoid

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map

import Control.Monad.Except
import Control.Monad.State

import Debug.Trace
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

qvarId    : varId      { $1 }
          | tqvarId    { Name  NsVar   (ModuleName $ fst $ identifierName $1) (snd $ identifierName $1) }
qconId    : conId      { $1 }
          | tqconId    { Name  NsCon   (ModuleName $ fst $ identifierName $1) (snd $ identifierName $1) }
qsymId    : symId      { $1 }
          | tqsymId    { Name  NsVar   (ModuleName $ fst $ identifierName $1) (snd $ identifierName $1) }
qsymconId : symconId   { $1 }
          | tqsymconId { Name  NsCon   (ModuleName $ fst $ identifierName $1) (snd $ identifierName $1) }
qtcId     : conId      { $1 }
          | tqconId    { Name  NsTyCls (ModuleName $ fst $ identifierName $1) (snd $ identifierName $1) }

qtyvarId    : varId      { $1 }
            | tqvarId    { Name NsVar (ModuleName $ fst $ identifierName $1) (snd $ identifierName $1) }
qtyconId    : conId      { $1 }
            | tqconId    { Name NsCon (ModuleName $ fst $ identifierName $1) (snd $ identifierName $1) }
qtysymId    : symId      { $1 }
            | tqsymId    { Name NsVar (ModuleName $ fst $ identifierName $1) (snd $ identifierName $1) }
qtysymconId : symconId   { $1 }
            | tqsymconId { Name NsCon (ModuleName $ fst $ identifierName $1) (snd $ identifierName $1) }

svarId : varId { $1 }
       | delimited('(', symId, ')') { $1 }
sconId : conId { $1 }
       | delimited('(', symconId, ')') { $1 }

qsvarId : qvarId { $1 }
       | delimited('(', qsymId, ')') { $1 }
qsconId : qconId { $1 }
       | delimited('(', qsymconId, ')') { $1 }
-- 

module : moduleHeader moduleBody EOF { let ModuleBody a b c d e = $2 in Module (fst $1) (snd $1) a b c d e }

moduleHeader : 'module' moduleName option(delimited('(', separated_list(export, ','), ')')) 'where'
                 { ($2, $3) }
             |   { (ModuleName ["Main"], Nothing) }

moduleBody :: { ModuleBody Name }
           : '{' list(';') separated_nonempty_list(importDeclaration, nonempty_list(';')) nonempty_list(';') separated_nonempty_list(topDeclaration, nonempty_list(';')) list(';') '}'
                  {% concatBody (ModuleBody $3 Map.empty Map.empty Map.empty [] : $5)
                     & maybe (error "pouet") return
                  }
           | '{' list(';') separated_nonempty_list(topDeclaration, nonempty_list(';')) list(';') '}'
                  {% concatBody $3
                     & maybe (error "pouet") return
                  }

moduleName :: { ModuleName }
           : qconId { let Name _ a b = $1 in a <> ModuleName [b] }

importDeclaration :: { ModuleImport Name }
                  : 'import' option('qualified') moduleName option(preceded('as', moduleName)) option(delimited('(', separated_list(import, ','), ')')) option(preceded('hiding', delimited('(', separated_list(import, ','), ')')))
                     { ModuleImport (isJust $2) $3 (maybe $3 id $4) $5 (maybe [] id $6) }

import :: { ModuleImportSpec Name }
import : varId
           { InpVar $1 }
       | conId '(' '..' ')'
           { InpAll $1 }
       | conId '(' separated_list(conId, ',') ')'
           { InpFilter $1 $3 }

export :: { ModuleExportSpec Name } 
export : varId
           { ExpVar $1 }
       | conId '(' '..' ')'
           { ExpAll $1 }
       | conId '(' separated_list(conId, ',') ')'
           { ExpFilter $1 $3 }
       | 'module' moduleName
           { ExpModule $2 }

topDeclaration : 'type' simpletype '=' type
                   { moduleBodyTypeDeclaration (fst $2) (TypeDeclaration (snd $2) $4) }
               | 'data' {- context -} simpletype '=' separated_nonempty_list(constructor, '|')
                    { moduleBodyTypeDeclaration (fst $2) (DataDeclaration (snd $2) $4) }
--                -- | 'newtype' context simpletype '=' conId list(atype) { 0 }
--                | 'class' {- simplecontext -} conId varId option(preceded('where', delimited('{', separated_list(classDeclaration, nonempty_list(';')), '}')))
--                     { [TopClassDeclaration $2 (ClassDeclaration $3 $ maybe Map.empty (Map.fromList . concat) $4 )] }
--                | 'instance' {- simplecontext -} qtcId instance option(preceded('where', delimited('{', separated_list(instanceDeclaration, nonempty_list(';')), '}')))
--                     {% do
--                       decls <- makeDeclarationMap $ maybe Map.empty (foldr addBinding Map.empty) $4
--                       return [TopInstanceDeclaration $2 (fst $3) (InstanceDeclaration (snd $3) decls)]
--                     }
--             -- | 'default' { 0 }
               | declaration { undefined }

declaration -- :: { Declaration Name }
            : genDeclaration             { undefined }
            | leftHandSide rightHandSide { undefined } -- [VariableDeclaration (fst $1) (snd $1) $2] }
         -- | pattern rightHandSide      { Map.empty {- Bind pattern variables -} }

genDeclaration -- :: { Declaration Name }
            -- : fixity option(integer) symId { 0 }
               : separated_nonempty_list(svarId, ',') '::' typeSignature { undefined } -- fmap (flip SignatureDeclaration $3) $1 }

fixity :: { Fixity }
       : 'infixl' { Infixl }
       | 'infixr' { Infixr }
       | 'infix'  { Infix }

leftHandSide :: { (Name, [Pat Name]) }
             : svarId {-nonempty_-}list(apattern) { ($1, $2) }

rightHandSide :: { Expr Name () } 
              : '=' expression                { $2 }
          --  | '|' expression '=' expression { 0 }

constructor :: { DataConstructor Name }
            : sconId list(atype)    { DataConstructor $1 $2 }
         -- | btype symconId btype { 0 }

-- ---- Types

typeSignature : type { $1 }
          --  | context conId list(varId) { 0 }

simpletype :: { (Name, [Name]) }
           : conId list(varId) { ($1, $2) }

type :: { Type Name () }
     : separated_nonempty_list(btype, '->')
        { foldr arrowType (last $1) (init $1) }

btype :: { Type Name () }
      : nonempty_list(atype) { makeTypeApplication (head $1) (tail $1) }

atype :: { Type Name () }
      : gtycon                                                  { $1 }
      | varId                                                   { ann $ TyVar' $1 }
      -- | delimited('(', separated_nonempty_list(type, ','), ')')
      --     { case length $1 of
      --         1 -> head $1
      --         _ -> makeTypeApplication
      --               (TyConstant (Name ["Primitive"] TypeNsCon (UserName (replicate (length $1 - 1) ','))))
      --               $1
      --     }
      -- | delimited('[', type, ']')                               { TyApplication (TyConstant (Name ["Primitive"] TypeNsCon (UserName "[]"))) $1 }

gtycon :: { Type Name () }
--       : '(' ')'                    { TyConstant (Name ["Primitive"] TypeNsCon (UserName "()")) }
--       | '(' nonempty_list(',') ')' { TyConstant (Name ["Primitive"] TypeNsCon (UserName (replicate (length $2) ','))) }
--       | '[' ']'                    { TyConstant (Name ["Primitive"] TypeNsCon (UserName "[]")) }
       : '(' '->' ')'                { ann $ TyCon' (primitiveTyCon "->") }
--       | qtyconId                   { TyConstant $1 }

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
         : qsymId         { ann $ EVar' $1 }
         | qsymconId      { ann $ EVar' $1 }
         | '`' qconId '`' { ann $ EVar' $2 }
         | '`' qvarId '`' { ann $ EVar' $2 }

expression :: { Expr Name () }
           : expression10                     { $1 }
           | expression operator expression10 { ann $ EApp' (ann $ EApp' $2 $1) $3 }

expression10 :: { Expr Name () }
             : 'if' expression 'then' expression 'else' expression
                  { ann $ ECase' $2
                    [ (PCon (Name NsCon (primitiveModule) "True") [] [], $4)
                    , (PCon (Name NsCon (primitiveModule) "False") [] [], $6)
                    ]
                    Nothing
                  }
             -- | '\\'  nonempty_list(apattern) '->' expression
             --      { makeLambda $2 $4 }
          --    | 'let' '{' list(';') separated_nonempty_list(declaration, nonempty_list(';')) list(';') '}' 'in' expression
          --         {% do
          --             decls <- makeDeclarationMap $ foldr addBinding Map.empty (concat $4)
          --             return $ ann $ ELet decls $8
          --         }
          --    | 'case' expression 'of' '{' separated_nonempty_list(option(casealternative), ';') '}'
          --         { ann $ ECase $2 (fmap fromJust $ filter isJust $ $5) }
          -- -- | 'do'
             | nonempty_list(aexpression) { foldl (\a b -> ann $ EApp' a b) (head $1) (tail $1) }

aexpression :: { Expr Name () }
            : qsvarId                                                       { ann $ EVar' $1 }
            | qsconId                                                       { ann $ EVar' $1 }
            | integer                                                       { ann $ EInt' (integerValue $1) }
            | char                                                          { ann $ EChar' (charValue $1) }
            -- | string
            --     { foldl
            --       (\a b -> ann $ EApp' (ann $ EApp' (ann $ EVar' (primitiveCon ":")) a) (ann $ EChar' b))
            --       (ann $ EVar' (primitiveCon "[]"))
            --       (stringValue $1)
            --     }
            -- | delimited('(', separated_list(expression, ','), ')')
            --     { case length $1 of
            --         0 -> ann $ EVar' (primitiveCon "()")
            --         1 -> head $1
            --         _ -> foldl (\a b -> ann $ EApp' a b) (ann $ EVar' (Name primitiveModule (replicate (length $1 - 1) ','))) $1
            --     }
            -- | delimited('[', separated_list(expression, ','), ']')
            --     { foldl
            --       (\a b -> ann $ EApp' (ann $ EApp' (ann $ EVar' (primitiveCon ":")) a) b)
            --       (ann $ EVar' (primitiveCon "[]"))
            --       $1
            --     }

casealternative : pattern '->' expression { ($1, $3) }
             -- | pattern '|' expression '->' expression { ($1, $5) }

-- Patterns

pattern : pattern10 { $1 }
        | pattern qsymconId pattern10 { PCon $2 [$1, $3] [] }

pattern10 : apattern { $1 }
          | qconId nonempty_list_(apattern)
              { PCon $1 $2 [] }

apattern : varId                        { PAny [$1] }
         | varId '@' apattern           { over patternVariables ($1 :) $3 }
         | '_'                          { PAny [] }
         | qconId                       { PCon $1 [] [] }
         -- | delimited('(', separated_list(pattern, ','), ')')
         --      { case $1 of
         --          []    -> Pattern (PConstructor (Name ["Primitive"] NsCon (UserName "()")) []) []
         --          [pat] -> pat
         --          ps    -> Pattern (PConstructor (Name ["Primitive"] NsCon (replicate (length $1 - 1) ',')) $1) []
         --      }
         -- | delimited('[', separated_list(pattern, ','), ']')
         --      { foldr
         --        (\x y -> Pattern (PConstructor (Name ["Primitive"] NsCon (UserName ":")) [x, y]) [])
         --        (Pattern (PConstructor (Name ["Primitive"] NsCon (UserName "[]")) []) [])
         --        $1
         --      }

{

data ParseError = ParseError
                deriving (Show)

type ParseMonad = ExceptT ParseError (State Int)

runParse :: ParseMonad a -> State Int (Either ParseError a)
runParse = runExceptT

-- generateName :: ParseMonad Name
-- generateName = do
--   n <- get
--   modify (+ 1)
--   return $ GeneratedName n

data Fixity = Infixl | Infixr | Infix

primitiveModule :: ModuleName
primitiveModule = ModuleName ["Primitive"]

primitiveVar :: String -> Name
primitiveVar = Name NsVar primitiveModule

primitiveCon :: String -> Name
primitiveCon = Name NsCon primitiveModule

primitiveTyCon :: String -> Name
primitiveTyCon = Name NsTyCon primitiveModule

-- ann = Locate noLocation

-- makeLambda :: [Pattern Name] -> Expr Name () -> Expr Name ()
-- makeLambda pats e = foldr (\pat e -> ann $ ELambda (UserName "_") $ ann $ ECase (ann $ EVar' (Name [] NsVar (UserName "_"))) [(pat, e)]) e pats

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
