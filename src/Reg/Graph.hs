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

import Debug.Trace

type Graph = Map CoreName (Set CoreName)

data Register = Physical MipsRegister
              | Stack Int
              deriving (Eq, Ord, Show)

availableRegisters :: Set Register
availableRegisters = Set.fromList $ Physical <$> [t0, t1, t2, t3, t4, t5, t6, t7, s0, s1, s2, s3, s4, s5, s6, s7, t8, t9]

makeGraph :: Expression -> Graph
makeGraph e = let g = Map.fromList $ fmap (\v -> (v, Set.empty)) (Set.toList $ expressionVariables e)
              in addGraphEdges g (Set.toList $ expressionVariables e)
  -- where
  --   makeGraph' e@(EApplication _ _) g    =
  --     let vs = Set.toList $ expressionFreeVariables e in
  --     addGraphEdges g vs
  --   makeGraph' (ELet bs e) g             =
  --     let vs = Set.toList $ Set.unions (fmap (\(_, _, e) -> expressionFreeVariables e) bs) in
  --     makeGraph' e
  --     $ addGraphEdges g vs
  --   makeGraph' e@(EDataCase a alts df) g =
  --     let vs = Set.toList $ expressionFreeVariables e in
  --     addGraphEdges g vs



addGraphEdges g vs = foldr (\(a, b) g -> addEdge g a b) g [(x, y) | x <- vs, y <- vs]
  where
    addEdge g a b | a == b = g
    addEdge g a b | a /= b = Map.update (Just . Set.insert b) a
                             $ Map.update (Just . Set.insert a) b
                             $ g
type Coloring = Map CoreName Register

colorGraph :: Graph -> Coloring -> Int -> (Int, Coloring)
colorGraph g pc i =
  let pck = Map.keysSet pc
      g' = Map.map (Set.filter (not . flip Set.member pck)) . Map.filterWithKey (\k _ -> not $ Set.member k pck) $ g
  in colorGraph' g' i
  where
    colorGraph' g i =
      let dgs = Map.map Set.size g in
      let (lk, hk) = Map.partition (<= Set.size availableRegisters) dgs in
      let ls  = Map.keysSet lk
          hs  = Map.keysSet hk
      in
      let (i', c) = if Set.null hs
              then
                (i, pc)
              else
                let (b, hs') = Set.deleteFindMin hs
                    g'       = Map.map (Set.filter (flip Set.member hs')) . Map.filterWithKey (\k _ -> Set.member k hs') $ g
                    (i', c') = colorGraph' g' (i+1)
                in (i', Map.insert b (Stack i) $ c')
       in ( i'
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
