module Reg.Graph where

import Syntax.Name

import Reg.Expression

import Backend.Mips

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Maybe
import Data.Char
import Data.List

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer

type Graph = Map CoreName (Set CoreName)

data Register = Physical MipsRegister
              | Stack Int
              deriving (Eq, Ord)

availableRegisters :: Set Register
availableRegisters = Set.fromList $ Physical <$> [t0, t1, t2, t3, t4, t5, t6, t7, s0, s1, s2, s3, s4, s5, s6, s7, t8, t9]

makeGraph :: Expression -> Graph
makeGraph e = makeGraph' e (Map.fromList $ fmap (\v -> (v, Set.empty)) (Set.toList $ expressionVariables e))
  where
    makeGraph' e@(EApplication _ _) g    =
      let vs = Set.toList $ expressionFreeVariables e in
      addEdges g vs
    makeGraph' (ELet bs e) g             =
      let vs = Set.toList $ Set.unions (fmap (\(_, _, e) -> expressionFreeVariables e) bs) in
      makeGraph' e
      $ addEdges g vs
    makeGraph' e@(EDataCase a alts df) g =
      let vs = Set.toList $ expressionFreeVariables e in
      addEdges g vs

    addEdge g a b | a == b = g
    addEdge g a b | a /= b =
      Map.update (Just . Set.insert b) a
      $ Map.update (Just . Set.insert a) b
      $ g

    addEdges g vs = foldr (\(a, b) g -> addEdge g a b) g (zip vs vs)

colorGraph :: Graph -> Int -> (Int, Map CoreName Register)
colorGraph g i | Map.null g = (i, Map.empty)
colorGraph g i =
    let dgs = Map.map Set.size g in
    let (lk, hk) = Map.partition (<= Set.size availableRegisters) dgs in
    let ls  = Map.keysSet lk
        hs  = Map.keysSet hk
    in
    let c = if Set.null hs
            then
              Map.empty
            else
              let (b, hs') = Set.deleteFindMin hs in
              let g' = Map.map (Set.filter (flip Set.member hs')) . Map.filterWithKey (\k _ -> Set.member k hs') $ g in
              let (i', c') = colorGraph g' (i+1) in
              Map.insert b (Stack i) $ c'
    in (i
       , Set.fold
         (\n c -> Map.insert
                  n
                  (Set.findMin $ Set.difference
                   availableRegisters
                   (Set.fromList $ fmap fromJust . filter isJust $ flip Map.lookup c <$> (Set.toList $ fromJust $ Map.lookup n g))
                  )
                  c
         )
         c
         ls
       )
