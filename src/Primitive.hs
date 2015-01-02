module Primitive where

import Syntax.Expression
import Syntax.Type
import Syntax.Name
import Syntax.Module

import Control.Applicative
import Control.Monad
import Control.Lens

import Data.List
import Data.Maybe
import Data.Monoid
import Data.Foldable

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map

primitiveModule :: Module Name
primitiveModule = Module
                  (ModuleName ["Primitive"])
                  Nothing
                  []
                  (Map.fromList [])
                  (Map.fromList $
                   [ ( localTyCon "()"
                     , DataDeclaration []
                         [ DataConstructor (localCon "()") []]
                     )
                   , ( localTyCon "[]"
                     , DataDeclaration [localTyVar "a"]
                         [ DataConstructor (localCon "[]") []
                         , DataConstructor (localCon ":")
                           [ tyVar (localTyVar "a")
                           , tyApp (tyCon (localTyCon "[]")) (tyVar (localTyVar "a"))
                           ]
                         ]
                     )
                   ] ++ ([1..4] <&> \i ->
                                      let vs = localTyVar . ("ty" ++) . show <$> [1..i+1] in
                                      ( localTyCon (replicate i ',')
                                      , DataDeclaration vs
                                        [ DataConstructor (localCon (replicate i ',')) (tyVar <$> vs) ]
                                      )
                        )
                  )
                  (Map.fromList [])
                  []
