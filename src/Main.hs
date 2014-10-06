module Main where

import Data.List
import Control.Monad
import Control.Monad.State
import Control.Applicative

import Syntax.Location
import Syntax.Name
import Syntax.Module
import Syntax.Small.Lexer
import Syntax.Small.Token
import Syntax.Small.Parser
import Primitive

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Desugar.Typecheck
import Desugar.Rename

import System.Environment

import Driver.Driver

isFlag :: String -> Bool
isFlag ('-':'-':_) = True
isFlag _           = False

parseOnly :: String -> IO ()
parseOnly fn = do
  putStrLn $ fn ++ " : "
  putStr "\t"
  str <- readFile fn
  case tokenize str of
    Left e -> putStrLn.show $ e
    Right ts -> do
      case runParser ts parseModule of
        Left e -> putStrLn.show $ e
        Right e -> putStrLn $ "GOOD"

typecheckOnly :: String -> IO ()
typecheckOnly fn = do
  putStrLn $ fn ++ " : "
  putStr "\t"
  str <- readFile fn
  case tokenize str of
    Left e   -> putStrLn.show $ e
    Right ts -> do
      case runParser ts parseModule of
        Left e   -> putStrLn.show $ e
        Right md -> do
          let r = typecheck (makeModuleMap [primitiveModule, md])
          putStrLn.show $ r


        --flip evalState 0 $ do
        --  a <- runRenameMonad Map.empty (renameModule md)
        --  case a of
        --    Left e  -> return (putStrLn.show $ e)
        --    Right a -> do
        --      b <- runTypecheckMonad (Environment Map.empty Map.empty) (typecheckModule a)
        --      case b of
        --        Left e  -> return (putStrLn.show $ e)
        --        Right a -> return (putStrLn.show $ a)


main :: IO ()
main = do
  (flags, filenames) <- partition isFlag <$> getArgs
  case ("--parse-only" `elem` flags, "--typecheck-only" `elem` flags) of
    (True, False) -> forM_ filenames parseOnly
    (False, True) -> forM_ filenames typecheckOnly
    _ -> putStrLn "Usage: cmd [--parse-only | --typecheck-only] FILENAMES..."