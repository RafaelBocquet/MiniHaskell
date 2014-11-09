{
module Syntax.Full.Parser where

import Syntax.Full.Token
import Syntax.Expression
import Syntax.Type
import Syntax.Name
import Syntax.Module
import Syntax.Location

import Control.Applicative
import Control.Monad

import Data.Maybe

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map

import Control.Monad.Except
import Control.Monad.State

import Debug.Trace
}

%name parseModule module
%tokentype { Token' }

%monad { ParseMonad }
%error { parseError }

%token 
  tvarId            { t | isVariableIdentifier t }
  tconId            { t | isConstructorIdentifier t }
  tsymId            { t | isSymbolIdentifier t }
  tsymconId         { t | isSymbolConstructorIdentifier t }

  tqvarId           { t | isQVariableIdentifier t }
  tqconId           { t | isQConstructorIdentifier t }
  tqsymId           { t | isQSymbolIdentifier t }
  tqsymconId        { t | isQSymbolConstructorIdentifier t }

  integer           { TkInteger _ }
  char              { TkChar _ }
  string            { TkString _ }


  'case'            { TkCase }
  'class'           { TkClass }
  'data'            { TkData }
  'default'         { TkDefault }
  'deriving'        { TkDeriving }
  'do'              { TkDo }
  'else'            { TkElse }

  'if'              { TkIf }
  'import'          { TkImport }
  'in'              { TkIn }
  'infix'           { TkInfix }
  'infixl'          { TkInfixl }
  'infixr'          { TkInfixr }
  'instance'        { TkInstance }

  'let'             { TkLet }
  'module'          { TkModule }
  'newtype'         { TkNewtype }
  'of'              { TkOf }
  'then'            { TkThen }
  'type'            { TkType }
  'where'           { TkWhere }
  '_'               { TkUnderscore }

  '..'              { TkDoubleDot }
  ':'               { TkColon }
  '::'              { TkDoubleColon }
  '='               { TkEqual }
  '\\'              { TkLambda }
  '|'               { TkPipe }
  '<-'              { TkLArrow }
  '->'              { TkRArrow }
  '@'               { TkAt }
  '~'               { TkTilde }
  '=>'              { TkFatArrow }

  '('               { TkLParen }
  ')'               { TkRParen }
  ','               { TkComma }
  ';'               { TkSemiColon }
  '['               { TkLBracket }
  ']'               { TkRBracket }
  '`'               { TkBackTick }
  '{'               { TkLBrace }
  '}'               { TkRBrace }

%nonassoc 'if' 'then' 'else' '\\' '->'
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

varId : tvarId { UserName (snd $ identifierName $1) }
conId : tconId { UserName (snd $ identifierName $1) }
symId : tsymId { UserName (snd $ identifierName $1) }
symconId : tsymconId { UserName (snd $ identifierName $1) }

qvarId : tvarId { QName (fst $ identifierName $1) VariableName (UserName $ snd $ identifierName $1) }
       | tqvarId { QName (fst $ identifierName $1) VariableName (UserName $ snd $ identifierName $1) }
qconId : tconId { QName (fst $ identifierName $1) ConstructorName (UserName $ snd $ identifierName $1) }
       | tqconId { QName (fst $ identifierName $1) ConstructorName (UserName $ snd $ identifierName $1) }
qsymId : tsymId { QName (fst $ identifierName $1) VariableName (UserName $ snd $ identifierName $1) }
       | tqsymId { QName (fst $ identifierName $1) VariableName (UserName $ snd $ identifierName $1) }
qsymconId : tsymconId { QName (fst $ identifierName $1) ConstructorName (UserName $ snd $ identifierName $1) }
          | tqsymconId { QName (fst $ identifierName $1) ConstructorName (UserName $ snd $ identifierName $1) }

qtyvarId : tvarId { QName (fst $ identifierName $1) TypeVariableName (UserName $ snd $ identifierName $1) }
         | tqvarId { QName (fst $ identifierName $1) TypeVariableName (UserName $ snd $ identifierName $1) }
qtyconId : tconId { QName (fst $ identifierName $1) TypeConstructorName (UserName $ snd $ identifierName $1) }
         | tqconId { QName (fst $ identifierName $1) TypeConstructorName (UserName $ snd $ identifierName $1) }
qtysymId : tsymId { QName (fst $ identifierName $1) TypeVariableName (UserName $ snd $ identifierName $1) }
         | tqsymId { QName (fst $ identifierName $1) TypeVariableName (UserName $ snd $ identifierName $1) }
qtysymconId : tsymconId { QName (fst $ identifierName $1) TypeConstructorName (UserName $ snd $ identifierName $1) }
            | tqsymconId { QName (fst $ identifierName $1) TypeConstructorName (UserName $ snd $ identifierName $1) }

svarId : varId { $1 }
       | delimited('(', symId, ')') { $1 }
sconId : conId { $1 }
       | delimited('(', symconId, ')') { $1 }

qsvarId : qvarId { $1 }
       | delimited('(', qsymId, ')') { $1 }
qsconId : qconId { $1 }
       | delimited('(', qsymconId, ')') { $1 }
-- 

module : moduleHeader moduleBody {% makeModule $1 $2 }

moduleHeader : 'module' moduleName 'where' { $2 }
             |                             { ["Main"] }

moduleBody :: { [TopDeclaration] }
           --: '{' list(';') separated_nonempty_list(importDeclaration, nonempty_list(';')) list(';') '}'
           --       { $3 }
           : '{' list(';') separated_nonempty_list(importDeclaration, nonempty_list(';')) nonempty_list(';') separated_nonempty_list(topDeclaration, nonempty_list(';')) list(';') '}'
                  { $3 ++ concat $5 }
           | '{' list(';') separated_nonempty_list(topDeclaration, nonempty_list(';')) list(';') '}'
                  { concat $3 }

moduleName :: { ModuleName }
           : qconId { let QName a _ (UserName b) = $1 in a ++ [b] }

importDeclaration :: { TopDeclaration }
                  : 'import' moduleName { ImportDeclaration $2 }

topDeclaration :: { [TopDeclaration] }
               : 'type' simpletype '=' type
                    { [TopTypeDeclaration (fst $2) (TypeDeclaration (snd $2) $4)] }
               | 'data' {-context-} simpletype '=' separated_nonempty_list(constructor, '|')
                    { [TopTypeDeclaration (fst $2) (DataDeclaration (snd $2) $4)] }
               --| 'newtype' context simpletype '=' conId list(atype) { 0 }
               --| 'class' simplecontext conId varId option(preceded('where', delimited('{', separated_list(classDeclaration, nonempty_list(';')), '}'))) { 0 }
               --| 'instance' simplecontext qconId instance option(preceded('where', delimited('{', separated_list(instanceDeclaration, nonempty_list(';')), '}'))) { 0 }
            -- | 'default' { 0 }
               | declaration { fmap TopVariableDeclaration $1 }

declaration :: { [VariableDeclaration] }
            : genDeclaration             { $1 }
            | leftHandSide rightHandSide { [VariableDeclaration (fst $1) (snd $1) $2] }
         -- | pattern rightHandSide      { Map.empty {- Bind pattern variables -} }

genDeclaration :: { [VariableDeclaration] }
            -- : fixity option(integer) symId { 0 }
               : separated_nonempty_list(svarId, ',') '::' typeSignature { fmap (flip SignatureDeclaration $3) $1 }

fixity :: { Fixity }
       : 'infixl' { Infixl }
       | 'infixr' { Infixr }
       | 'infix'  { Infix }

leftHandSide :: { (SyntaxName, [Pattern SyntaxName]) }
             : svarId nonempty_list(apattern) { ($1, $2) }

rightHandSide :: { Expression SyntaxName } 
              : '=' expression                { $2 }
          --  | '|' expression '=' expression { 0 }

constructor :: { DataConstructor SyntaxName }
            : sconId list(atype)    { DataConstructor $1 $2 }
         -- | btype symconId btype { 0 }

---- Types

typeSignature : type { $1 }
          --  | context conId list(varId) { 0 }

simpletype :: { (SyntaxName, [SyntaxName]) }
           : conId list(varId) { ($1, $2) }

type :: { MonoType SyntaxName }
     : separated_nonempty_list(btype, '->')
        { foldr (\a b -> makeTypeApplication TyArrow [a, b]) (last $1) (init $1) }

btype :: { MonoType SyntaxName }
      : nonempty_list(atype) { makeTypeApplication (head $1) (tail $1) }

atype :: { MonoType SyntaxName }
      : gtycon                                                  { $1 }
      | varId                                                   { TyVariable $1 }
      | delimited('(', separated_nonempty_list(type, ','), ')')
          { case length $1 of
              1 -> head $1
              _ -> makeTypeApplication
                    (TyConstant (QName ["Primitive"] TypeConstructorName (UserName (replicate (length $1 - 1) ','))))
                    $1
          }
      | delimited('[', type, ']')                               { TyApplication (TyConstant (QName ["Primitive"] TypeConstructorName (UserName "[]"))) $1 }

gtycon :: { MonoType SyntaxName }
       : '(' ')'                    { TyConstant (QName ["Primitive"] TypeConstructorName (UserName "()")) }
       | '(' nonempty_list(',') ')' { TyConstant (QName ["Primitive"] TypeConstructorName (UserName (replicate (length $2) ','))) }
       | '[' ']'                    { TyConstant (QName ["Primitive"] TypeConstructorName (UserName "[]")) }
       | '(' '->' ')'               { TyArrow }
       | qtyconId                   { TyConstant $1 }

---- Classes, context

context : class '=>'                                           { 0 }
        | delimited('(', separated_list(class, ','), ')') '=>' { 0 }

simplecontext : simpleclass '=>'                                           { 0 }
              | delimited('(', separated_list(simpleclass, ','), ')') '=>' { 0 }

class : qconId varId                      { 0 }
      | qconId delimited('(', atype, ')') { 0 }

simpleclass : qconId varId                      { 0 }

classDeclaration : genDeclaration             { 0 }
                 | leftHandSide rightHandSide { 0 }
                 | varId rightHandSide        { 0 }

instance : gtycon                                                { 0 }
         | '(' gtycon list(varId) ')'                            { 0 }
         | '(' varId ',' separated_nonempty_list(varId, ',') ')' { 0 }
         | '[' varId ']'                                         { 0 }
         | '(' qtyvarId '->' qtyvarId ')'                        { 0 }

instanceDeclaration : leftHandSide rightHandSide { 0 }
                    | varId rightHandSide        { 0 }

-- Expressions

operator :: { Expression SyntaxName }
         : qsymId         { lll $ EVariable $1 }
         | qsymconId      { lll $ EVariable $1 }
         | '`' qconId '`' { lll $ EVariable $2 }
         | '`' qvarId '`' { lll $ EVariable $2 }

expression :: { Expression SyntaxName }
           : expression10                     { $1 }
           | expression operator expression10 { lll $ EApplication (lll $ EApplication $2 $1) $3 }

expression10 :: { Expression SyntaxName }
          -- : 'if' expression 'then' expression 'else' expression { 0 }
             : '\\'  nonempty_list(apattern) '->' expression
                  { makeLambda $2 $4 }
          -- | 'let'
             | 'case' expression 'of' '{' separated_nonempty_list(option(casealternative), ';') '}'
                  { lll $ ECase $2 (fmap fromJust $ filter isJust $ $5) }
          -- | 'do'
             | nonempty_list(aexpression) { foldl (\a b -> lll $ EApplication a b) (head $1) (tail $1) }

aexpression :: { Expression SyntaxName }
            : qsvarId                                                       { lll $ EVariable $1 }
            | qsconId                                                       { lll $ EVariable $1 }
            | integer                                                       { lll $ EInteger (integerValue $1) }
            | char                                                          { lll $ EChar (charValue $1) }
            | string
                { foldl
                  (\a b -> lll $ EApplication (lll $ EApplication (lll $ EVariable (QName ["Primitive"] ConstructorName (UserName ":"))) a) (lll $ EChar b))
                  (lll $ EVariable (QName ["Primitive"] ConstructorName (UserName "[]")))
                  (stringValue $1)
                }
            | delimited('(', separated_list(expression, ','), ')')
                { case length $1 of
                    0 -> lll $ EVariable (QName ["Primitive"] ConstructorName (UserName "()"))
                    1 -> head $1
                    _ -> foldl (\a b -> lll $ EApplication a b) (lll $ EVariable (QName ["Primitive"] ConstructorName (UserName $ replicate (length $1 - 1) ','))) $1
                }
            | delimited('[', separated_list(expression, ','), ']')
                { foldl
                  (\a b -> lll $ EApplication (lll $ EApplication (lll $ EVariable (QName ["Primitive"] ConstructorName (UserName ":"))) a) b)
                  (lll $ EVariable (QName ["Primitive"] ConstructorName (UserName "[]")))
                  $1
                }

casealternative : pattern '->' expression { ($1, $3) }
             -- | pattern '|' expression '->' expression { ($1, $5) }

-- Patterns

pattern : pattern10 { $1 }
        | pattern qsymconId pattern10 { Pattern (PConstructor $2 [$1, $3]) [] }

pattern10 : apattern { $1 }
          | qconId nonempty_list_(apattern)
              { Pattern (PConstructor $1 $2) [] }

apattern : varId                        { Pattern PWildcard [$1] }
         | varId '@' apattern           { let Pattern pat as = $3 in Pattern pat ($1 : as) }
         | '_'                          { Pattern PWildcard [] }
         | qconId                       { Pattern (PConstructor $1 []) [] }
         | delimited('(', separated_list(pattern, ','), ')')
              { case $1 of
                  []    -> Pattern (PConstructor (QName ["Primitive"] ConstructorName (UserName "()")) []) []
                  [pat] -> pat
                  ps    -> Pattern (PConstructor (QName ["Primitive"] ConstructorName (UserName $ replicate (length $1 - 1) ',')) $1) []
              }
         | delimited('[', separated_list(pattern, ','), ']')
              { foldr
                (\x y -> Pattern (PConstructor (QName ["Primitive"] ConstructorName (UserName ":")) [x, y]) [])
                (Pattern (PConstructor (QName ["Primitive"] ConstructorName (UserName "[]")) []) [])
                $1
              }


{

data ParseError = ParseError
                deriving (Show)

type ParseMonad = ExceptT ParseError (State Int)

runParse :: ParseMonad a -> State Int (Either ParseError a)
runParse = runExceptT

generateName :: ParseMonad SyntaxName
generateName = do
  n <- get
  modify (+ 1)
  return $ GeneratedName n

lll = Locate noLocation

makeLambda :: [Pattern SyntaxName] -> Expression SyntaxName -> Expression SyntaxName
makeLambda pats e = foldr (\pat e -> lll $ ELambda (UserName "_") $ lll $ ECase (lll $ EVariable (QName [] VariableName (UserName "_"))) [(pat, e)]) e pats

data VariableDeclaration = VariableDeclaration SyntaxName [Pattern SyntaxName] (Expression SyntaxName)
                         | SignatureDeclaration SyntaxName (MonoType SyntaxName)
                         | PatternDeclaration (Pattern SyntaxName) (Expression SyntaxName)

type BindingMap = Map SyntaxName (Maybe (MonoType SyntaxName), [([Pattern SyntaxName], Expression SyntaxName)])

addBinding :: VariableDeclaration -> BindingMap -> BindingMap
addBinding (VariableDeclaration n p e) m = case Map.lookup n m of
  Nothing               -> Map.insert n (Nothing, [(p, e)]) m
  Just (t, [])          -> Map.insert n (t, [(p, e)]) m
  Just (t, es@((p', e'):_))
    | length p' == length p -> Map.insert n (t, (p, e):es) m
    | otherwise -> error "Bad pattern length ..."
addBinding (SignatureDeclaration n t) m = case Map.lookup n m of
  Nothing           -> Map.insert n (Just t, []) m
  Just (Just _, _)  -> error "Multiple signatures"
  Just (Nothing, e) -> Map.insert n (Just t, e) m
-- addBinding (PatternDeclaration p e) m = case 

makeDeclarationMap :: BindingMap -> ParseMonad (DeclarationMap SyntaxName)
makeDeclarationMap mp =
    Map.fromList <$> (\(a, b) -> do
      b' <- makeDeclaration b
      return (a, b')
    ) `mapM` Map.toList mp
  where
    makeDeclaration (t, e@((pats, _):_)) = do
      let ne = length pats
      ns <- forM [1..ne] $ const generateName
      let decl = foldr
            (((.).(.)) lll ELambda)
            (if ne == 1
              then
                lll $ ECase
                (lll . EVariable . QName [] VariableName $ head ns)
                (fmap (\(pat, e) -> (head pat, e)) e)
              else
                lll $ ECase
                (makeApplication (lll $ EVariable (QName ["Primitive"] ConstructorName (UserName $ replicate (ne - 1) ','))) (lll . EVariable . QName [] VariableName <$> ns))
                (fmap (\(pat, e) -> (Pattern (PConstructor (QName ["Primitive"] ConstructorName (UserName $ replicate (ne - 1) ',')) pat) [], e)) e)
            )
            ns
      return $ Declaration t decl

data TopDeclaration = ImportDeclaration ModuleName
                    | TopVariableDeclaration VariableDeclaration
                    | TopTypeDeclaration SyntaxName (TypeDeclaration SyntaxName)

makeModule :: ModuleName -> [TopDeclaration] -> ParseMonad (Module SyntaxName)
makeModule n tds = do
  let (m, bs) = foldl
        (flip addTopDeclaration)
        (Module n Set.empty Map.empty Map.empty, Map.empty)
        tds
  ds <- makeDeclarationMap bs
  return $ m { moduleDeclarations = ds }
  where
    addTopDeclaration (ImportDeclaration impName) (m, bs) = (m { moduleImport = Set.insert impName (moduleImport m) }, bs)
    addTopDeclaration (TopVariableDeclaration v) (m, bs)  = (m, addBinding v bs)
    addTopDeclaration (TopTypeDeclaration a b) (m, bs)    = (m { moduleTypeDeclarations = Map.insert a b (moduleTypeDeclarations m) }, bs)

parseError x = error $ "Parse error : " ++ show x

}
