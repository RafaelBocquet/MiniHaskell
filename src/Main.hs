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

import Core.Pretty as C

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

parseOnlySmall :: String -> IO ()
parseOnlySmall fn = do
  putStrLn $ fn ++ " : "
  putStr "\t"
  str <- readFile fn
  case Small.tokenise str of
    Left e -> putStrLn.show $ e
    Right ts -> do
      case Small.runParser ts Small.parseModule of
        Left e -> putStrLn.show $ e
        Right e -> putStrLn $ "GOOD"


typecheckOnlySmall :: [String] -> IO ()
typecheckOnlySmall fn = do
  putStrLn $ show fn ++ " : "
  putStr "\t"
  str1:strs <- readFile `mapM` fn
  flip evalState 0 $ do
    case Small.tokenise str1 of
      Left e -> return (putStrLn . show $ e)
      Right t1 -> case Small.runParser t1 Small.parseModule of
        Left e -> return (putStrLn . show $ e)
        Right p1 -> do
          ps <- (Full.runParse . Full.parseModule . Full.tokenise) `mapM` strs
          case foldr (flip f) (Right []) ps of
            Left e -> return (putStrLn.show $ e)
            Right ps -> do
              r <- typecheck (makeModuleMap $ primitiveModule : p1 : ps)
              return $ putStrLn . show $ r
  where
    f (Left e) _           = Left e
    f (Right rs) (Left e)  = Left e
    f (Right rs) (Right r) = Right $ r : rs

typecheckOnlyFull :: [String] -> IO ()
typecheckOnlyFull fn = do
  putStrLn $ show fn ++ " : "
  putStr "\t"
  strs <- readFile `mapM` fn
  flip evalState 0 $ do
    ps <- (Full.runParse . Full.parseModule . Full.tokenise) `mapM` strs
    case foldr (flip f) (Right []) ps of
      Left e -> return (putStrLn.show $ e)
      Right ps -> do
        r <- typecheck (makeModuleMap $ primitiveModule : ps)
        return $ putStrLn.show $ r
  where
    f (Left e) _           = Left e
    f (Right rs) (Left e)  = Left e
    f (Right rs) (Right r) = Right $ r : rs

compileFull' :: [[Full.Token']] -> IO ()
compileFull' strs = do
  flip evalState 0 $ do
    ps <- (Full.runParse . Full.parseModule) `mapM` strs
    case foldr (flip f) (Right []) ps of
      Left e -> return (putStrLn.show $ e)
      Right ps -> do
        r <- typecheck (makeModuleMap $ primitiveModule : ps)
        r' <- compile r
        return $ putStrLn.show $ r'
  where
    f (Left e) _           = Left e
    f (Right rs) (Left e)  = Left e
    f (Right rs) (Right r) = Right $ r : rs

compileFull :: [String] -> IO ()
compileFull fn = do
  -- putStrLn $ show fn ++ " : "
  -- putStr "\t"
  strs <- readFile `mapM` fn
  flip evalState 0 $ do
    ps <- (Full.runParse . Full.parseModule . Full.tokenise) `mapM` strs
    case foldr (flip f) (Right []) ps of
      Left e -> return (putStrLn.show $ e)
      Right ps -> do
        r <- typecheck (makeModuleMap $ primitiveModule : ps)
        return $ putStrLn . C.runPretty . mapM_ id . fmap C.prettyCoreModule . Map.elems $ r
        r' <- compile r
        return $ putStrLn.show $ r'
  where
    f (Left e) _           = Left e
    f (Right rs) (Left e)  = Left e
    f (Right rs) (Right r) = Right $ r : rs
          
main :: IO ()
main = do
  (flags, filenames) <- partition isFlag <$> getArgs
  case ("--parse-only" `elem` flags, "--type-only" `elem` flags, "--full" `elem` flags) of
    (True, False, False)   -> parseOnlySmall (head filenames)
    (False, True, False)   -> typecheckOnlySmall filenames
    -- (True, False, True) -> forM_ filenames parseOnlyFull
    (False, True, _)       -> typecheckOnlyFull filenames
    (False, False, _)      -> compileFull filenames
    _                      -> putStrLn "Usage: cmd [--parse-only | --typecheck-only] [--full] FILENAMES...\nUnless the flag --full is used, the first file in filenames is considered to be a small haskell file."
