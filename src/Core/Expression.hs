module Core.Expression where

data Expression' = EInteger Integer
                 | EChar Char
                 | EBool Bool