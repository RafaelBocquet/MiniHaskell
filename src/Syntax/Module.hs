module Syntax.Module where

import Syntax.Location
import Syntax.Name
import Syntax.Expression

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

type ModuleName = [String]

--data ImportDeclaration = ImportDeclaration
--  { importModule    :: ModuleName
--  --, importQualified :: Bool  
--  --, importAs        :: Maybe ModuleName
--  }

data Module n = Module
  { moduleName             :: ModuleName
  , moduleImport           :: Set ModuleName
  , moduleDataDeclarations :: DataDeclarationMap n
  , moduleDeclarations     :: DeclarationMap n
  }

makeModuleMap :: [Module n] -> Map ModuleName (Module n)
makeModuleMap = Map.fromList . fmap (\m -> (moduleName m, m))