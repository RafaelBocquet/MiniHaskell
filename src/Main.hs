{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Primitive

import Syntax.Expression
import Syntax.Type
import Syntax.Module
import Syntax.PrettyPrint

import Rename.Monad
import Rename.Expression
import Rename.Module

import Typecheck.Expression

import Parsing.Parser
import Parsing.Monad
import Parsing.Lexer

import Control.Applicative
import Control.Monad
import Control.Monad.Except

import Data.List

import System.IO
import System.Environment

import Text.PrettyPrint.HughesPJ hiding ((<>), empty)
import Text.PrettyPrint.HughesPJClass hiding ((<>), empty)

--

data AnyError = AParseError ParseError
              | ALexingError LexingError
              deriving (Show)

instance Pretty AnyError where
  pPrint (AParseError a)  = pPrint a
  pPrint (ALexingError a) = pPrint a

class DriverError a where
  driverError :: a -> AnyError
instance DriverError ParseError where
  driverError = AParseError
instance DriverError LexingError where
  driverError = ALexingError

newtype DriverMonad a = DriverMonad { unDriverMonad :: Except AnyError a }
                        deriving (Functor, Applicative, Monad, MonadError AnyError)

liftDriver :: DriverError b => Either b a -> DriverMonad a
liftDriver (Left e)  = throwError (driverError e)
liftDriver (Right a) = return a

runDriver :: DriverMonad a -> Either AnyError a
runDriver = runExcept . unDriverMonad

doCompile :: String -> IO ()
doCompile fn = do
  src <- readFile fn
  let a = runDriver $ do
        toks <- liftDriver $ tokenise src
        mod  <- liftDriver $ runParseMonad $ parseModule toks
        let rmod = runRename $ renameModule primitiveModule
        return rmod
  putStrLn.show.pPrint $ a

isFlag :: String -> Bool
isFlag ('-':'-':_) = True
isFlag _           = False

main :: IO ()
main = do
  (flags, filenames) <- partition isFlag <$> getArgs
  doCompile (head filenames)
  
