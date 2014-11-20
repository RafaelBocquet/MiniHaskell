module Syntax.Module where

import Syntax.Location
import Syntax.Name
import Syntax.Expression
import Syntax.Type

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

type ModuleName = [String]

-- TODO : Move this to Parser.y

data Module n = Module
  { moduleName              :: ModuleName
  , moduleImport            :: Set ModuleName
  , moduleTypeDeclarations  :: TypeDeclarationMap n
  , moduleClassDeclarations :: ClassDeclarationMap n
  , moduleDeclarations      :: DeclarationMap n
  }
  deriving (Show)

makeModuleMap :: [Module n] -> Map ModuleName (Module n)
makeModuleMap = Map.fromList . fmap (\m -> (moduleName m, m))
