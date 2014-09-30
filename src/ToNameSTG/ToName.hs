module ToNameSTG.ToName where

type Variable   = Int
type Tag        = Int

data Expression = ELet [Int] Int Expression
                | EAbstractCase  {- Alts -} [(Tag, Int, Expression)] {- Default -} Expression
                | EPrimitiveCase {- Alts -} [(Int, Expression)]      {- Default -} Expression