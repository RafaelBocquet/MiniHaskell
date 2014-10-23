module Main where

import Data.List
import Control.Monad
import Control.Monad.Writer
import Control.Monad.State
import Control.Applicative

import Syntax.Location
import Syntax.Name
import Syntax.Module
import qualified Syntax.Small.Lexer as Small
import qualified Syntax.Small.Token as Small
import qualified Syntax.Small.Parser as Small
import qualified Syntax.Full.Lexer as Full (tokenise)
import qualified Syntax.Full.Token as Full
import qualified Syntax.Full.Parser as Full
import Primitive

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Desugar.Typecheck
import Desugar.Rename

import System.Environment

import Driver.Driver

import Backend.Mips
import Backend.Runtime

isFlag :: String -> Bool
isFlag ('-':'-':_) = True
isFlag _           = False

parseOnly :: String -> IO ()
parseOnly fn = do
  putStrLn $ fn ++ " : "
  putStr "\t"
  str <- readFile fn
  case Small.tokenise str of
    Left e -> putStrLn.show $ e
    Right ts -> do
      case Small.runParser ts Small.parseModule of
        Left e -> putStrLn.show $ e
        Right e -> putStrLn $ "GOOD"

typecheckOnly :: String -> IO ()
typecheckOnly fn = do
  putStrLn $ fn ++ " : "
  putStr "\t"
  str <- readFile fn
  case Small.tokenise str of
    Left e   -> putStrLn.show $ e
    Right ts -> do
      case Small.runParser ts Small.parseModule of
        Left e   -> putStrLn.show $ e
        Right md -> 
          flip evalState 0 $ do
            r <- typecheck (makeModuleMap [primitiveModule, md])
            return $ putStrLn.show $ r

parseOnlyFull :: String -> IO ()
parseOnlyFull fn = do
  putStrLn $ fn ++ " : "
  putStr "\t"
  str <- readFile fn
  case Full.tokenise str of
    ts -> do
      putStrLn.show $ ts
      flip evalState 0 $ do
        ps <- Full.runParse $ Full.parseModule ts
        case ps of
          Left e -> return (putStrLn.show $ e)
          Right ps -> return (putStrLn.show $ ps)

typecheckOnlyFull :: String -> IO ()
typecheckOnlyFull fn = do
  putStrLn $ fn ++ " : "
  putStr "\t"
  str <- readFile fn
  case Full.tokenise str of
    ts -> do
      putStrLn.show $ ts
      flip evalState 0 $ do
        ps <- Full.runParse $ Full.parseModule ts
        case ps of
          Left e -> return (putStrLn.show $ e)
          Right ps -> do
            r <- typecheck (makeModuleMap [primitiveModule, ps])
            return $ putStrLn.show $ r

main :: IO ()
main = do
  (flags, filenames) <- partition isFlag <$> getArgs
  case ("--parse-only" `elem` flags, "--typecheck-only" `elem` flags, "--full" `elem` flags) of
    (True, False, False) -> forM_ filenames parseOnly
    (False, True, False) -> forM_ filenames typecheckOnly
    (True, False, True) -> forM_ filenames parseOnlyFull
    (False, True, True) -> forM_ filenames typecheckOnlyFull
    _ -> putStrLn "Usage: cmd [--parse-only | --typecheck-only] [--full] FILENAMES..."
