module Main where

import Data.List
import Control.Monad
import Control.Applicative

import Syntax.Location
import Syntax.Name
import Syntax.Small.Lexer
import Syntax.Small.Token
import Syntax.Small.Parser

import Desugar.Typecheck

import System.Environment

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
      -- putStrLn.show $ ts
      case runNameMonad $ runParser ts parseModule of
        Left e -> putStrLn.show $ e
        Right e -> putStrLn $ "GOOD"

typecheckOnly :: String -> IO ()
typecheckOnly fn = do
  putStrLn $ fn ++ " : "
  putStr "\t"
  str <- readFile fn
  case tokenize str of
    Left e   -> putStrLn.show $ e
    Right ts -> runNameMonad $ do
      v <- runParser ts parseModule
      case v of
        Left e   -> return.putStrLn.show $ e
        Right md -> do
          v <- runTypecheckMonad (typecheckModule md)
          case v of
            Left e   -> return.putStrLn.show $ e
            Right bs -> return.putStrLn.show $ bs

main :: IO ()
main = do
  (flags, filenames) <- partition isFlag <$> getArgs
  case ("--parse-only" `elem` flags, "--typecheck-only" `elem` flags) of
    (True, False) -> forM_ filenames parseOnly
    (False, True) -> forM_ filenames typecheckOnly
    _ -> putStrLn "Usage: cmd [--parse-only | --typecheck-only] FILENAMES..."