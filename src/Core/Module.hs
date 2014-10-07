module Core.Module where

import Core.Expression

data Module = Module [String] DeclarationMap
            deriving (Show)

moduleDeclarationMap :: Module -> DeclarationMap
moduleDeclarationMap (Module _ d) = d