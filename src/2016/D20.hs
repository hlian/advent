#!/usr/bin/env stack
-- stack --resolver lts-7.14 --install-ghc runghc --package lens --package base-prelude
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ViewPatterns      #-}
module D20 where
import           BasePrelude hiding ((&), left)
import           Control.Lens
import           D20Input
import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import           Data.Set (Set)
import qualified Data.Set as Set


solution input =
  ""

windows row =
  filter ((== 2) . length) $ map (take 2) (tails row)

example =
  [(-1, -1), (11, 11)]

ok [(a, b), (c, d)] =
  b + 1 < c

overlap (a, b) (c, d) =
  b >= c - 1 && a <= d

merge whole@((a, b):(c, d):rest) =
  if overlap (a, b) (c, d) then merge ((a,max b d):rest) else ((a,b):merge ((c,d):rest))
merge xs | length xs <= 1 = xs

diff [(a, b), (c, d)] =
  if c >= b + 1 then ((c - 1) - (b + 1) + 1) else error "whoa"

f (a, b) (l, h, acc) | a > h + 1 = (a, b, (l,h):acc)
                     | overlap (a, b) (l, h) = (l, b, acc)
                     | otherwise = (a, max b h, acc)

solve2 _ n [] = n
solve2 blocked n ((low,hi):rest)
    | low > blocked+1 = solve2 (max blocked hi) (n+low-blocked-1) rest
    | otherwise       = solve2 (max blocked hi) n rest

main = do
  -- print (sort example)
  -- print (filter ok $ windows (sort example))
  -- print (filter ok $ windows (sort input))
  let n = 2200000000000000
  -- print (take n $ sort input)
  print (sum $ map diff . windows . merge $ take n $ sort input)
  -- print (4294967296 :: Integer)
