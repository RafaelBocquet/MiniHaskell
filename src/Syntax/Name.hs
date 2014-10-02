module Syntax.Name where

import Data.Char
import Control.Monad
import Control.Monad.State

data SyntaxName = UserName String | GeneratedName Int
                deriving (Eq, Ord)
instance Show SyntaxName where
  show (UserName s@(c:_)) | isAlpha c = s
  show (UserName s)                   = "(" ++ s ++ ")"
  show (GeneratedName i)              = "?" ++ show i

data CoreName   = CoreName Int String
instance Eq CoreName where
  CoreName i _ == CoreName j _ = i == j
instance Ord CoreName where
  CoreName i _ `compare` CoreName j _ = i `compare` j
instance Show CoreName where
  show (CoreName _ s) = s

data NameSpace = VariableName
               | ConstructorName
               deriving (Eq, Ord)

data Name n = Name NameSpace n
            deriving (Eq, Ord)

instance Show n => Show (Name n) where
  show (Name _ s) = show s

data QName n = QName [String] (Name n)
           deriving (Eq, Ord)

instance Show n => Show (QName n) where
  show (QName ms n) = concat (fmap (++ ".") ms) ++ show n
