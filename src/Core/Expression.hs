module Core.Expression where

import Syntax.Type

data Expression' = EInteger Integer
                 | EChar Char
                 | EBool Bool
                 | EVariable Int
                 | EApplication Expression Expression
                 | ELambda Int Expression
                 | ETuple [Expression]
                 | EIf Expression Expression Expression
                 | ELet BindingMap Expression
                 | EListCase Expression Expression Int Int Expression
data Expression = Expression MonoType Expression'

data BindingMap = Map Int (PolyType, Expression)