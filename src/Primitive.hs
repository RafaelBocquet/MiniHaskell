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
      [ (UserName "Int#",  PrimitiveDataDeclaration UnboxedIntDataDeclaration)
      , (UserName "Char#", PrimitiveDataDeclaration UnboxedCharDataDeclaration)
      , (UserName "Bool",  DataDeclaration []             [DataConstructor (UserName "True") [], DataConstructor (UserName "False") []])
      , (UserName "()",    DataDeclaration []             [DataConstructor (UserName "()") []])
      , (UserName "IO",    PrimitiveDataDeclaration IODataDeclaration)
      ] -- ++ ((\n -> (UserName (replicate (n-1) ','), PrimitiveDataDeclaration (TupleDataDeclaration n))) <$> [2..62])
  , moduleDeclarations     = Map.fromList
      [ (UserName "+#",      PrimitiveDeclaration PrimitiveIntAdd)
      , (UserName "-#",      PrimitiveDeclaration PrimitiveIntSub)
      , (UserName "*#",      PrimitiveDeclaration PrimitiveIntMul)
      , (UserName "div#",    PrimitiveDeclaration PrimitiveIntDiv)
      , (UserName "rem#",    PrimitiveDeclaration PrimitiveIntRem)
      , (UserName "negate#", PrimitiveDeclaration PrimitiveIntNegate)
      , (UserName "intLT#",  PrimitiveDeclaration PrimitiveIntLT)
      , (UserName "intLE#",  PrimitiveDeclaration PrimitiveIntLE)
      , (UserName "intGT#",  PrimitiveDeclaration PrimitiveIntGT)
      , (UserName "intGE#",  PrimitiveDeclaration PrimitiveIntGE)
      , (UserName "intEQ#",  PrimitiveDeclaration PrimitiveIntEQ)
      , (UserName "intNE#",  PrimitiveDeclaration PrimitiveIntNE)

      , (UserName "ord#",     PrimitiveDeclaration PrimitiveOrd)
      , (UserName "chr#",     PrimitiveDeclaration PrimitiveChr)
      , (UserName "charLT#",  PrimitiveDeclaration PrimitiveCharLT)
      , (UserName "charLE#",  PrimitiveDeclaration PrimitiveCharLE)
      , (UserName "charGT#",  PrimitiveDeclaration PrimitiveCharGT)
      , (UserName "charGE#",  PrimitiveDeclaration PrimitiveCharGE)
      , (UserName "charEQ#",  PrimitiveDeclaration PrimitiveCharEQ)
      , (UserName "charNE#",  PrimitiveDeclaration PrimitiveCharNE)
      ]
  }