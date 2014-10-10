module Base where

import Syntax.Name
import Syntax.Module
import Syntax.Expression
import Syntax.Type
import Syntax.Location

import Control.Applicative

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

ll = Locate noLocation

baseModule :: Module SyntaxName
baseModule = Module
  { moduleName             = ["Base"]
  , moduleImport           = Set.fromList [["Primitive"]]
  , moduleDataDeclarations = Map.fromList $
      [ (UserName "Int",   DataDeclaration [] [DataConstructor (UserName "I#") [TyConstant (QName ["Primitive"] TypeConstructorName (UserName "Int#"))]])
      , (UserName "Char",  DataDeclaration [] [DataConstructor (UserName "C#") [TyConstant (QName ["Primitive"] TypeConstructorName (UserName "Char#"))]])
      , (UserName "[]",    DataDeclaration
                             [UserName "a"]
                             [ DataConstructor (UserName "[]") []
                             , DataConstructor (UserName ":")
                                [ TyVariable (UserName "a")
                                , TyApplication (TyConstant (QName ["Base"] TypeConstructorName (UserName "[]"))) (TyVariable (UserName "a"))
                                ]
                             ]
        )
      ] -- ++ ((\n -> (UserName (replicate (n-1) ','), PrimitiveDataDeclaration (TupleDataDeclaration n))) <$> [2..62])
  , moduleDeclarations     = Map.fromList
      [ ( UserName "fromInteger"
        , Declaration $ ll $ ELambda (UserName "x") (ll $ EVariable (QName [] VariableName (UserName "x")))
        )
      , ( UserName "/="
        , Declaration $ ll $
            ELambda (UserName "x") $ ll $
            ELambda (UserName "y") $ ll $
            ECase (ll $ EVariable (QName [] VariableName (UserName "x")))
            [ ( PConstructor (QName ["Base"] ConstructorName (UserName "I#")) [PAs (UserName "x'") PWildcard]
              , ll $ ECase (ll $ EVariable (QName [] VariableName (UserName "y")))
                  [ ( PConstructor (QName ["Base"] ConstructorName (UserName "I#")) [PAs (UserName "y'") PWildcard]
                    , ll $ EApplication
                        (ll $ EVariable (QName ["Base"] ConstructorName (UserName "I#")))
                        (ll $ EApplication (ll $ EApplication () (ll $ EVariable)) ())
                    )
                  ]
              )
            ]
        )
      ]
  }
