#!/usr/bin/env stack
-- stack --resolver lts-6.26 --install-ghc runghc --package base-prelude --package monad-loops
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE OverloadedLists #-}

module D11 where
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Set (Set)
import BasePrelude hiding (try, (&))
import Control.Monad.Loops (unfoldrM)
import Control.Lens
import Data.Map (Map)

import D11Input

neighbors :: [(Int, [Furniture Element], Bool)] -> [State]
neighbors st =
  f (max (level - 1) 0) <> f (min (level + 1) 3)
  where
    floor = head (filter (\(_, _, x) -> x == True) st)
    level :: Int
    level = floor ^. _1
    f :: Int -> [State]
    f next = do
      guard (next /= level)
      let st' =
            st & ix level . _3 .~ False & ix next . _3 .~ True
      let choices = st' ^?! ix level . _2
      carried <- carry choices
      let st'' = st' & ix level . _2 %~ filter (`notElem` carried)
                     & ix next . _2 <>~ carried
      guard (isValid (st'' ^?! ix level))
      guard (isValid (st'' ^?! ix next))
      pure st''

dfs :: (Eq a, Ord a, Show a) => a -> (a -> [a]) -> (a -> Bool) -> ([a], Int)
dfs start expand success =
  f Set.empty [start] 0
  where
    f seen frontier steps = do
      let good = Set.filter success frontier
      if (Set.null good) then do
        let expansion = Set.fromList $ concatMap expand (Set.toList frontier)
        let expansion' = Set.difference expansion seen
        let ugh = Map.fromList [(expansion, n) | n <- Set.toList frontier, expansion <- expand n]
        let (crumbs, ok) = f (Set.union seen frontier) expansion' (steps + 1)
        ((fromJust $ ugh ^. at (head crumbs)):crumbs, ok)
      else
        ([head $ Set.toList good], steps)

isValid floor =
  length (unmatchedChips) == 0 || length unmatchedGens == 0
  where
    chips = floor ^.. _2 . traverse . _M
    gens = floor ^.. _2 . traverse . _G
    unmatchedGens = filter (`notElem` chips) gens
    unmatchedChips = filter (`notElem` gens) chips

done :: Int -> State -> Bool
done count st =
  length (st ^?! ix 3 . _2) == count

done2 :: Int -> State -> Bool
done2 count st =
  st == me2

format :: State -> IO ()
format st = do
  mapM_ print st
  print "---"

main = do
  mapM format (neighbors example)
  print (isValid goodFloor)
  let (steps, count) = dfs input neighbors (done inputCount)
  mapM format steps
  print count

carry :: [Furniture Element] -> [[Furniture Element]]
carry lst@(h:rest) =
  let carry' = carry rest in
  [[a, h] | [a] <- carry'] <> [[h]] <> carry'
carry [a] = [[a]]
carry [] = []
carry _ =
  error "impossible carry"
