module Parsing.Layout where

import Parsing.Token
import Syntax.Location

data LToken = LToken Token
            | LOpen Int Int
            | LWhite Int Int
            deriving (Show)

ltokenise :: [Token] -> [LToken]
ltokenise ts@(Token TkModule _ _ : _) = ltokenise' ts
ltokenise ts@(Token _ _ i : _)        = LOpen 1 i : ltokenise' ts
ltokenise []                          = []

ltokenise' :: [Token] -> [LToken]
ltokenise' ts@(t : Token TkLBrace _ _ : _)
  | isOpeningToken t = ltokenise'' ts
ltokenise' ts@(t : _)
  | isOpeningToken t =
      case ltokenise'' ts of
       t : ts'@(LToken (Token TkEOF _ i) : _) -> t : LOpen i 0 : ts'
       t : ts'@(LToken (Token _ _ i) : _)     -> t : LOpen i i : ts'
       t : ts'@(LOpen i _ : _)                -> t : LOpen i i : ts'
       t : ts'@(LWhite i _ : _)               -> t : LOpen i i : ts'
  | otherwise = ltokenise'' ts
ltokenise' []        = []

--    ltokenise'' (t : ts@(t' : _)) | &
ltokenise'' []  = []
ltokenise'' [t] = [LToken t]
ltokenise'' (t@(tokenLocation -> Location (Position a r c) l) : ts@((tokenLocation -> Location (Position a' r' c') l') : _))
  | r < r' = LToken t : LWhite c' c' : ltokenise' ts 
  | otherwise           = LToken t : ltokenise' ts

isOpeningToken (Token t _ _) = t `elem` [TkLet, TkOf, TkWhere, TkDo, TkOf]

layout :: [LToken] -> [Int] -> [Token]
layout (LWhite p n : ts)                   (m : ms)  | n == m = Token TkSemiColon noLocation 0 : layout ts                (m : ms)
                                                     | n < m = Token TkRBrace    noLocation 0 : layout (LWhite p n : ts) ms
layout (LOpen p n : ts)                    (m : ms)  | n > m = Token TkLBrace    noLocation 0 : layout ts                (n : m : ms)
layout (LOpen p n : ts)                    []        | n > 0 = Token TkLBrace    noLocation 0 : layout ts                (n : [])
layout (LToken t@(tokenToken -> TkRBrace) : ts) (0 : ms)      = t : layout ts ms
layout (LToken t@(tokenToken -> TkLBrace) : ts) ms            = t : layout ts (0 : ms)
layout [LToken t@(tokenToken -> TkEOF)]     ms | all (> 0) ms = fmap (const $ Token TkRBrace noLocation 0) ms ++ [t]
layout (LToken t : ts)                     ms                = t : layout ts ms
layout err ms = error (show (err, ms))

makeLayout :: [Token] -> [Token]
makeLayout ts = layout (ltokenise ts) []
