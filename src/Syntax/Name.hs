{-# LANGUAGE FlexibleContexts #-}

module Syntax.Name where

import Data.Char
import Control.Monad
import Control.Monad.State

data NameId = UserName String | GenName Int
            deriving (Eq, Ord)

instance Show NameId where
  show (UserName s@(c:_)) | isAlpha c = s
  show (UserName s)                   = "(" ++ s ++ ")"
  show (GenName i)                    = "?" ++ show i

data NameSpace = VariableName
               | ConstructorName
               deriving (Eq, Ord)

data Name   = Name NameSpace NameId
            deriving (Eq, Ord)

instance Show Name where
  show (Name _ s) = show s

data QName  = QName [String] Name
            deriving (Eq, Ord)

instance Show QName where
  show (QName ms n) = concat (fmap (++ ".") ms) ++ show n

runNameMonad :: State NameId a -> a
runNameMonad = flip evalState (GenName 0)

generateName :: MonadState NameId m => m NameId
generateName = do
  GenName cur <- get
  put (GenName $ cur + 1)
  return $ GenName cur