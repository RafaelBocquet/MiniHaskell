module Driver.Driver where

import Syntax.Module
import Syntax.Name

import qualified Core.Module as C

import Desugar.Rename
import Desugar.Typecheck

import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State

import Data.Maybe
import Data.Monoid
import Data.Foldable

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Debug.Trace

typecheck :: Map ModuleName (Module SyntaxName) -> Map ModuleName C.Module
typecheck moduleMap =
    let mods = reverse $ topologicalSort (Map.keysSet moduleMap) [] in flip evalState 0 $ do
    renaming <- runRenameMonad Map.empty $
      foldlM
        (\(renameMap, modMap) mod -> traceShow renameMap $ do
          let localRenameMap = Map.mapKeysWith mappend (\(QName _ n) -> QName [] n) renameMap
          (renameMap', mod') <- local (const $ Map.unionWith mappend renameMap localRenameMap) $ renameModule (fromJust $ Map.lookup mod moduleMap)
          return (Map.union renameMap' renameMap, Map.insert mod mod' modMap)
        )
        (Map.empty, Map.empty)
        mods
    case renaming of
      Left e                    -> error (show e)
      Right (renameMap, modMap) -> do
        typechecked <- runTypecheckMonad (Environment Map.empty renameMap) $
          foldlM
            (\cModuleMap mod -> do
              cMod <- typecheckModule (fromJust $ Map.lookup mod modMap)
              return $ Map.insert mod cMod cModuleMap
            )
            Map.empty
            mods
        case typechecked of
          Left e  -> error (show e)
          Right r -> return r
  where
    dependenciesMapStep :: Map ModuleName (Set ModuleName) -> Map ModuleName (Set ModuleName)
    dependenciesMapStep mp = Map.mapWithKey (\k v -> Set.union v $ Set.unions $ fromJust . flip Map.lookup mp <$> Set.toList v) mp

    makeDependenciesMap :: Int -> Map ModuleName (Set ModuleName) -> Map ModuleName (Set ModuleName)
    makeDependenciesMap 0 = id
    makeDependenciesMap n = makeDependenciesMap (n `div` 2) . dependenciesMapStep

    dependenciesMap :: Map ModuleName (Set ModuleName)
    dependenciesMap = makeDependenciesMap (Map.size moduleMap) (Map.map (Set.intersection (Set.fromList $ Map.keys moduleMap) . moduleImport) moduleMap)

    reverseDependenciesMap :: Map ModuleName (Set ModuleName)
    reverseDependenciesMap = Map.foldWithKey (\k -> flip . Set.fold . Map.update $ Just . Set.insert k) (Map.map (const Set.empty) moduleMap) dependenciesMap

    topologicalSort :: Set ModuleName -> [ModuleName] -> [ModuleName]
    topologicalSort st acc
      | Set.null st = acc
      | otherwise   =
          let elem    = Set.findMin st
              elemDep = fromJust $ Map.lookup elem dependenciesMap
            in
          if Set.member elem elemDep
            then error "Module cycle"
            else topologicalSort (Set.delete elem $ st `Set.difference` elemDep) (elem : topologicalSort elemDep acc)