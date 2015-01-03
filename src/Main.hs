{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Primitive

import Syntax.Module
import Syntax.PrettyPrint
import Syntax.Name

import Rename.Monad
import Rename.Module

import Typecheck.Expression

import Parsing.Parser
import Parsing.Monad
import Parsing.Lexer

import Control.Applicative
import Control.Monad
import Control.Monad.Except
import Control.Lens

import Data.List
import Data.Foldable

import System.FilePath
import System.IO
import System.Environment

import Text.PrettyPrint.HughesPJ hiding ((<>), empty)
import Text.PrettyPrint.HughesPJClass hiding ((<>), empty)

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map


--

data AnyError = AParseError ParseError
              | ALexingError LexingError
              | ARenameError RenameError
              deriving (Show)

instance Pretty AnyError where
  pPrint (AParseError a)  = pPrint a
  pPrint (ALexingError a) = pPrint a
  pPrint (ARenameError a) = pPrint a

class DriverError a where
  driverError :: a -> AnyError
instance DriverError ParseError where
  driverError = AParseError
instance DriverError LexingError where
  driverError = ALexingError
instance DriverError RenameError where
  driverError = ARenameError

newtype Driver a = Driver { unDriver :: Except AnyError a }
                 deriving (Functor, Applicative, Monad, MonadError AnyError)

liftDriver :: DriverError b => Either b a -> Driver a
liftDriver (Left e)  = throwError (driverError e)
liftDriver (Right a) = return a

runDriver :: Driver a -> Either AnyError a
runDriver = runExcept . unDriver

data ModuleSource = SourceFile String 
                  | SourcePrimitive (Module Name)


compileModuleSource :: Map ModuleName (Module UniqueName) -> ModuleSource -> Driver (Module UniqueName)
compileModuleSource mp (SourceFile src) = do
  toks <- liftDriver $ tokenise src
  mod  <- liftDriver $ runParseMonad $ parseModule toks
  liftDriver $ runRenameModule mp mod
compileModuleSource mp (SourcePrimitive mod) =do
  liftDriver $ runRenameModule mp mod

doCompile :: [ModuleSource] -> IO ()
doCompile mds = do
  let rmds = runDriver
             $ foldlM (\mp mod -> do
                          rmod <- compileModuleSource mp mod
                          return $ Map.insert (rmod ^. moduleName) rmod mp
                      ) Map.empty mds
  putStrLn.show.pPrint $ Map.toList <$> rmds

isFlag :: String -> Bool
isFlag ('-':'-':_) = True
isFlag _           = False

main :: IO ()
main = do
  (flags, filenames) <- partition isFlag <$> getArgs
  sourceFiles <- fmap SourceFile <$> forM filenames readFile
  doCompile (SourcePrimitive primitiveModule : sourceFiles)
  
