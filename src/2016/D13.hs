#!/usr/bin/env stack
-- stack --resolver lts-6.26 --install-ghc runghc --package base-prelude
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLists   #-}
module D13 where
import           Data.Set (Set)
import qualified Data.Set as Set
import           Numeric (showIntAtBase)
import           BasePrelude

type State = (Int, Int)

isOpen input (x, y) =
  even . length . filter (== '1') . repr $ blend input (x, y)
  where
    repr x = showIntAtBase 2 intToDigit x ""
    blend input (x, y) = x*x + 3*x + 2*x*y + y + y*y + input

neighbors input (x, y) =
  filter valid [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
  where
    valid (x, y) = (x >= 0) && (y >= 0) && isOpen input (x, y)

bfs :: Monoid a => Int -> (State -> Bool) -> (Set State -> a) -> Int -> a
bfs input success score depth =
  rec Set.empty [(1, 1)] 0
  where
    rec visited frontier steps
      | (depth == steps) = score frontier
      | otherwise = do
          if Set.null (Set.filter success frontier) then do
            let visited' = Set.union visited frontier
            let next = Set.fromList $ concatMap (neighbors input) (Set.toList frontier)
            let frontier' = Set.difference next visited'
            score frontier <> rec visited' frontier' (steps + 1)
          else
            mempty

solution1 input goal =
  bfs input (== goal) (const (Sum 1)) 100
solution2 input limit =
  bfs input (const False) (Sum . Set.size) limit
(example, input) = (10, 1350)
main = do
  print $ solution1 example (7, 4)
  print $ solution1 input (31, 39)
  print $ solution2 example 2
  print $ solution2 input 50
