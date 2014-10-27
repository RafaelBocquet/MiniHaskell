module Reg.Expression where

data Expression = EInteger Integer
                | EChar Char
                | ELocal
                | EGlobal
                | EApplication
                | ELet
                | ECase
