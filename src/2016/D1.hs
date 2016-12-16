#!/usr/bin/env stack
-- stack --resolver lts-6.26 --install-ghc runghc --package text --package base-prelude

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ViewPatterns      #-}

module D1 where

import qualified Data.Text as Text
import BasePrelude


data Direction =
  R | L
input =
  "R3, L2, L2, R4, L1, R2, R3, R4, L2, R4, L2, L5, L1, R5, R2, R2, L1, R4, R1, L5, L3, R4, R3, R1, L1, L5, L4, L2, R5, L3, L4, R3, R1, L3, R1, L3, R3, L4, R2, R5, L190, R2, L3, R47, R4, L3, R78, L1, R3, R190, R4, L3, R4, R2, R5, R3, R4, R3, L1, L4, R3, L4, R1, L4, L5, R3, L3, L4, R1, R2, L4, L3, R3, R3, L2, L5, R1, L4, L1, R5, L5, R1, R5, L4, R2, L2, R1, L5, L4, R4, R4, R3, R2, R3, L1, R4, R5, L2, L5, L4, L1, R4, L4, R4, L4, R1, R5, L1, R1, L5, R5, R1, R1, L3, L1, R4, L1, L4, L4, L3, R1, R4, R1, R1, R2, L5, L2, R4, L1, R3, L5, L2, R5, L4, R5, L5, R3, R4, L3, L3, L2, R2, L5, L5, R3, R4, R3, R4, R3, R1"
steps =
  [case direction of
     'R' -> (R, read rest :: Int)
     'L' -> (L, read rest :: Int)
  | direction:rest <- map Text.unpack (Text.splitOn ", " input)]
turn compass dir =
  mod (compass + case dir of R -> 1; L -> -1) 4
solution1 =
  join $
    scanl (\before (direction, n) ->
             case last before of
               (compass, x, y) -> case turn compass direction of
                 0 -> [(0, x, y + i) | i <- [0 .. n]]
                 1 -> [(1, x + i, y) | i <- [0 .. n]]
                 2 -> [(2, x, y - i) | i <- [0 .. n]]
                 3 -> [(3, x - i, y) | i <- [0 .. n]]
          ) [(0, 0, 0)] steps
solution2 =
  duplicates . map positionOf $ solution1
positionOf (_, x, y) =
  (x, y)
duplicates hay =
  hay !! fst (head dups)
  where
    dups = [(e, ix) | (e, ix) <- zip [0..] ixs, ix /= e, ix + 1 /= e]
    ixs  = [i | needle <- hay, Just i <- [elemIndex needle hay]]
main = do
  print (last solution1)
  case last solution1 of (_, x, y) -> print $ x + y
  print solution2
  case solution2 of (x, y) -> print $ x + y
