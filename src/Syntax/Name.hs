module Syntax.Name where

import Data.Char
import Control.Monad
import Control.Monad.State

data SyntaxName = UserName String | GeneratedName Int
                deriving (Eq, Ord)
instance Show SyntaxName where
  show (UserName s@(c:_)) | isAlpha c || c == '(' = s
  show (UserName s)                               = "(" ++ s ++ ")"
  show (GeneratedName i)                          = "?" ++ show i

data CoreName   = CoreName Int String
instance Eq CoreName where
  CoreName i _ == CoreName j _ = i == j
instance Ord CoreName where
  CoreName i _ `compare` CoreName j _ = i `compare` j
instance Show CoreName where
  show (CoreName i s@(c:_)) | isAlpha c || c == '(' = s ++ "#" ++ show i
                            | otherwise             = "(" ++ s ++ ")" ++ "#" ++ show i
  show (CoreName i "")                              = "#" ++ show i

data NameSpace = VariableName
               | ConstructorName
               | TypeVariableName
               | TypeConstructorName
               deriving (Eq, Ord, Show)

data QName n = QName [String] NameSpace n
           deriving (Eq, Ord)

instance Show n => Show (QName n) where
  show (QName ms ns n) = concat (fmap (++ ".") ms) ++ show n

type QSyntaxName = QName SyntaxName
type QCoreName   = QName CoreName