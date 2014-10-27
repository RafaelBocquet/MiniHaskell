module Core.Module where

import Core.Expression

data Module = Module
  { moduleName               :: [String]
  , moduleDataDeclarationMap :: DataDeclarationMap
  , moduleDeclarationMap     :: DeclarationMap
  }
  deriving (Show)

-- Type -> Machine Type
-- (Function kind) -> Arity to machine type
-- Primitive -> Unboxed
-- Other (Data type : boxed)
