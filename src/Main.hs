module Main where

import Data.List
import Control.Monad
import Control.Monad.Writer
import Control.Monad.State
import Control.Applicative

import Control.Exception (catch, SomeException)

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
import System.IO
import System.Exit (exitWith, ExitCode(..))

import Driver.Driver

import Backend.Mips
import Backend.Runtime

import Syntax.Full.Layout

isFlag :: String -> Bool
isFlag ('-':'-':_) = True
isFlag _           = False

parseOnlySmall :: String -> IO ()
parseOnlySmall fn = do
  str <- readFile fn
  case Small.tokenise str of
    Left e -> do
      putStrLn . Small.showLexError fn $ e
      exitWith (ExitFailure 1)
    Right ts -> do
      case Small.runParser ts Small.parseModule of
        Left e -> do
          putStrLn . Small.showParseError fn $ e
          exitWith (ExitFailure 1)
        Right e -> return ()


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
          let tks = Full.tokenise <$> strs
          case foldr (flip f) (Right []) tks of
           Left e -> return (putStrLn.show $ e)
           Right tks -> do
             ps <- (Full.runParse . Full.parseModule) `mapM` tks
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
    let tks = Full.tokenise <$> strs
    case foldr (flip f) (Right []) tks of
     Left e -> return (putStrLn.show $ e)
     Right tks -> do
       ps <- (Full.runParse . Full.parseModule) `mapM` tks
       case foldr (flip f) (Right []) ps of
        Left e -> return (putStrLn.show $ e)
        Right ps -> do
          r <- typecheck (makeModuleMap $ primitiveModule : ps)
          return $ putStrLn.show $ r
  where
    f (Left e) _           = Left e
    f (Right rs) (Left e)  = Left e
    f (Right rs) (Right r) = Right $ r : rs


compileSmall :: [String] -> IO ()
compileSmall fn = do
  str1:strs <- readFile `mapM` fn
  flip evalState 0 $ do
    case Small.tokenise str1 of
      Left e -> return (putStrLn . show $ e)
      Right t1 -> case Small.runParser t1 Small.parseModule of
        Left e -> return (putStrLn . show $ e)
        Right p1 -> do
          let tks = Full.tokenise <$> strs
          case foldr (flip f) (Right []) tks of
           Left e -> return (putStrLn.show $ e)
           Right tks -> do
             ps <- (Full.runParse . Full.parseModule) `mapM` tks
             case foldr (flip f) (Right []) ps of
              Left e -> return (putStrLn.show $ e)
              Right ps -> do
                r <- typecheck (makeModuleMap $ primitiveModule : p1 : ps)
                r' <- compile r
                return $ do
                  hPutStrLn stderr . C.runPretty . mapM_ id . fmap C.prettyCoreModule . Map.elems $ r
                  putStrLn.show $ r'
  where
    f (Left e) _           = Left e
    f (Right rs) (Left e)  = Left e
    f (Right rs) (Right r) = Right $ r : rs

compileFull :: [String] -> IO ()
compileFull fn = do
  strs <- readFile `mapM` fn
  flip evalState 0 $ do
    let tks = Full.tokenise <$> strs
    case foldr (flip f) (Right []) tks of
     Left e -> return (putStrLn.show $ e)
     Right tks -> do
       ps <- (Full.runParse . Full.parseModule) `mapM` tks
       case foldr (flip f) (Right []) ps of
        Left e -> return (putStrLn.show $ e)
        Right ps -> do
          r <- typecheck (makeModuleMap $ primitiveModule : ps)
          r' <- compile r
          return $ do
            -- hPutStrLn stderr . C.runPretty . mapM_ id . fmap C.prettyCoreModule . Map.elems $ r
            putStrLn.show $ r'
  where
    f (Left e) _           = Left e
    f (Right rs) (Left e)  = Left e
    f (Right rs) (Right r) = Right $ r : rs


main :: IO ()
main = -- flip catch (\e -> let _ = e :: SomeException in exitWith $ ExitFailure 2) $
       do
  (flags, filenames) <- partition isFlag <$> getArgs
  case ("--parse-only" `elem` flags, "--type-only" `elem` flags, "--full" `elem` flags) of
    (True, False, False)  | not (null filenames) -> parseOnlySmall (head filenames)
    (False, True, False)  | not (null filenames) -> typecheckOnlySmall filenames
    (False, False, False) | not (null filenames) -> compileSmall filenames
    (False, True, True)   | not (null filenames) -> typecheckOnlyFull filenames
    (False, False, True)  | not (null filenames) -> compileFull filenames
    _                     -> putStrLn "Usage: cmd [--parse-only | --type-only] [--full] FILENAMES...\nUnless the flag --full is used, the first file in filenames is considered to be a small haskell file."
