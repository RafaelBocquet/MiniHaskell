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

tupleVariables = fmap (\i -> UserName $ 't' : show i) [1..]

primitiveModule :: Module SyntaxName
primitiveModule = Module
  { moduleName             = ["Primitive"]
  , moduleImport           = Set.empty
  , moduleTypeDeclarations = Map.fromList $
      [ (UserName "Int_prim",  PrimitiveDataDeclaration UnboxedIntDataDeclaration)
      , (UserName "Char_prim", PrimitiveDataDeclaration UnboxedCharDataDeclaration)
      , (UserName "Bool",      DataDeclaration [] [DataConstructor (UserName "True") [], DataConstructor (UserName "False") []])
      , (UserName "()",        DataDeclaration [] [DataConstructor (UserName "()") []])
      , (UserName "",          DataDeclaration [] [DataConstructor (UserName "") []])
      , (UserName "IO",        PrimitiveDataDeclaration IODataDeclaration)
      ] ++ ((\n ->
              ( UserName (replicate (n-1) ',')
              , DataDeclaration (take n tupleVariables) [DataConstructor (UserName (replicate (n-1) ',')) $ TyVariable <$> take n tupleVariables]
              )
           ) <$> [2..4])
  , moduleDeclarations     = Map.fromList
      [ (UserName "add_prim",    PrimitiveDeclaration PrimitiveIntAdd)
      , (UserName "sub_prim",    PrimitiveDeclaration PrimitiveIntSub)
      , (UserName "mul_prim",    PrimitiveDeclaration PrimitiveIntMul)
      , (UserName "div_prim",    PrimitiveDeclaration PrimitiveIntDiv)
      , (UserName "rem_prim",    PrimitiveDeclaration PrimitiveIntRem)
      , (UserName "negate_prim", PrimitiveDeclaration PrimitiveIntNegate)
      , (UserName "intLT_prim",  PrimitiveDeclaration PrimitiveIntLT)
      , (UserName "intLE_prim",  PrimitiveDeclaration PrimitiveIntLE)
      , (UserName "intGT_prim",  PrimitiveDeclaration PrimitiveIntGT)
      , (UserName "intGE_prim",  PrimitiveDeclaration PrimitiveIntGE)
      , (UserName "intEQ_prim",  PrimitiveDeclaration PrimitiveIntEQ)
      , (UserName "intNE_prim",  PrimitiveDeclaration PrimitiveIntNE)

      , (UserName "ord_prim",     PrimitiveDeclaration PrimitiveOrd)
      , (UserName "chr_prim",     PrimitiveDeclaration PrimitiveChr)
      , (UserName "charLT_prim",  PrimitiveDeclaration PrimitiveCharLT)
      , (UserName "charLE_prim",  PrimitiveDeclaration PrimitiveCharLE)
      , (UserName "charGT_prim",  PrimitiveDeclaration PrimitiveCharGT)
      , (UserName "charGE_prim",  PrimitiveDeclaration PrimitiveCharGE)
      , (UserName "charEQ_prim",  PrimitiveDeclaration PrimitiveCharEQ)
      , (UserName "charNE_prim",  PrimitiveDeclaration PrimitiveCharNE)
      ]
  }
