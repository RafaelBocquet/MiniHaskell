module Core.Module where

import Core.Expression

data Module = Module [String] DeclarationMap
            deriving (Show)