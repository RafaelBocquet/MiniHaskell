module Primitive where

import Syntax.Name
import Syntax.Module
import Syntax.Expression
import Syntax.Type
import Syntax.Location

import Control.Applicative

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

primitiveModule :: Module SyntaxName
primitiveModule = Module
  { moduleName             = ["Primitive"]
  , moduleImport           = Set.empty
  , moduleDataDeclarations = Map.fromList $
      [ (UserName "->",    PrimitiveDataDeclaration ArrowDataDeclaration)
      , (UserName "Int",   PrimitiveDataDeclaration IntDataDeclaration)
      , (UserName "Char",  PrimitiveDataDeclaration CharDataDeclaration)
      , (UserName "Bool",  DataDeclaration []             [DataConstructor (UserName "True") [], DataConstructor (UserName "False") []])
      , (UserName "[]",    DataDeclaration
                             [UserName "a"]
                             [ DataConstructor (UserName "[]") []
                             , DataConstructor (UserName ":")
                                [ TyVariable (UserName "a")
                                , TyApplication (TyConstant (QName ["Primitive"] TypeConstructorName (UserName "[]"))) (TyVariable (UserName "a"))
                                ]
                             ]
        )
      , (UserName "()",    DataDeclaration []             [DataConstructor (UserName "()") []])
      , (UserName "IO",    PrimitiveDataDeclaration IODataDeclaration)
      ] -- ++ ((\n -> (UserName (replicate (n-1) ','), PrimitiveDataDeclaration (TupleDataDeclaration n))) <$> [2..62])
  , moduleDeclarations     = Map.fromList
      [ (UserName "fromInteger", PrimitiveDeclaration FromIntegerDeclaration)
      , (UserName "negate",      PrimitiveDeclaration NegateDeclaration)
      , (UserName "+",           PrimitiveDeclaration AddDeclaration)
      , (UserName "-",           PrimitiveDeclaration SubDeclaration)
      , (UserName "*",           PrimitiveDeclaration MulDeclaration)
      , (UserName "div",         PrimitiveDeclaration DivDeclaration)
      , (UserName "rem",         PrimitiveDeclaration RemDeclaration)
      , (UserName "<",           PrimitiveDeclaration LTDeclaration)
      , (UserName "<=",          PrimitiveDeclaration LEDeclaration)
      , (UserName ">",           PrimitiveDeclaration GTDeclaration)
      , (UserName ">=",          PrimitiveDeclaration GEDeclaration)
      , (UserName "==",          PrimitiveDeclaration EQDeclaration)
      , (UserName "/=",          PrimitiveDeclaration NEDeclaration)
      , (UserName "&&",          PrimitiveDeclaration AndDeclaration)
      , (UserName "||",          PrimitiveDeclaration OrDeclaration)
      , (UserName ">>=",         PrimitiveDeclaration BindDeclaration)
      , (UserName "return",      PrimitiveDeclaration ReturnDeclaration)
      , (UserName "error",       PrimitiveDeclaration ErrorDeclaration)
      , (UserName "putChar",     PrimitiveDeclaration PutCharDeclaration)
      ]
  }