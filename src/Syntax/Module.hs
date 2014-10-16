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

data VariableDeclaration = VariableDeclaration SyntaxName [Pattern SyntaxName] (Expression SyntaxName)
                         | SignatureDeclaration SyntaxName (MonoType SyntaxName)
                         | PatternDeclaration (Pattern SyntaxName) (Expression SyntaxName)

type BindingMap = Map SyntaxName (Maybe (MonoType SyntaxName), [([Pattern SyntaxName], Expression SyntaxName)])

addBinding :: VariableDeclaration -> BindingMap -> BindingMap
addBinding (VariableDeclaration n p e) m = case Map.lookup n m of
  Nothing               -> Map.insert n (Nothing, [(p, e)]) m
  Just (t, [])          -> Map.insert n (t, [(p, e)]) m
  Just (t, es@((p', e'):_))
    | length p' == length p -> Map.insert n (t, (p, e):es) m
    | otherwise -> error "Bad pattern length ..."
addBinding (SignatureDeclaration n t) m = case Map.lookup n m of
  Nothing           -> Map.insert n (Just t, []) m
  Just (Just _, _)  -> error "Multiple signatures"
  Just (Nothing, e) -> Map.insert n (Just t, e) m
-- addBinding (PatternDeclaration p e) m = case 

makeDeclarationMap :: BindingMap -> DeclarationMap SyntaxName
makeDeclarationMap =
  Map.map
  (\(t, e@((_, ne):_) ->
    let ne = length ne in
    -- \x1 x2 ... xne -> case (x1, x2, ..., xne) of (pats) -> (exprs)
    Declaration t $ Locate noLocation $ ECase () ()
  )

data TopDeclaration = ImportDeclaration ModuleName
                    | TopVariableDeclaration VariableDeclaration
                    | TopTypeDeclaration SyntaxName (TypeDeclaration SyntaxName)

data Module n = Module
  { moduleName             :: ModuleName
  , moduleImport           :: Set ModuleName
  , moduleTypeDeclarations :: TypeDeclarationMap n
  , moduleDeclarations     :: DeclarationMap n
  }
  deriving (Show)

makeModuleMap :: [Module n] -> Map ModuleName (Module n)
makeModuleMap = Map.fromList . fmap (\m -> (moduleName m, m))

makeModule :: ModuleName -> [TopDeclaration] -> Module SyntaxName
makeModule n tds =
  let (m, bm) = foldl
      (flip addTopDeclaration)
      (Module n Set.empty Map.empty Map.empty, Map.empty)
      tds
    in
  m { moduleDeclarations = makeDeclarationMap bm }
  where
    addTopDeclaration (ImportDeclaration impName) (m, bm) = (m { moduleImport = Set.insert impName (moduleImport m) }, bm)
    addTopDeclaration (TopVariableDeclaration v) (m, bm)  = (m, addBinding v bm)
    addTopDeclaration (TopTypeDeclaration a b) (m, bm)    = (m { moduleTypeDeclarations = Map.insert a b (moduleTypeDeclarations m) }, bm)
  
