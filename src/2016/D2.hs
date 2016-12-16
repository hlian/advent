#!/usr/bin/env stack
-- stack --resolver lts-6.26 --install-ghc runghc --package text --package base-prelude

{-# LANGUAGE NoImplicitPrelude #-}

module D2 where

import qualified Data.Text as Text
import qualified Data.List as List

import           BasePrelude
import           D2Input

lock2 =
  [ (1, [(D, 3)])
  , (2, [(R, 3), (D, 6)])
  , (3, [(U, 1), (L, 2), (R, 4), (D, 7)])
  , (4, [(L, 3), (D, 8)])
  , (5, [(R, 6)])
  , (6, [(U, 2), (L, 5), (D, 10), (R, 7)])
  , (7, [(L, 6), (R, 8), (U, 3), (D, 11)])
  , (8, [(L, 7), (R, 9), (U, 4), (D, 12)])
  , (9, [(L, 8)])
  , (10, [(U, 6), (R, 11)])
  , (11, [(L, 10), (U, 7), (R, 12), (D, 13)])
  , (12, [(L, 11), (U, 8)])
  , (13, [(U, 11)])
  ]

cap n = min 2 (max n 0)
turn (x, y) U = (x, cap (y + 1))
turn (x, y) D = (x, cap (y - 1))
turn (x, y) L = (cap (x - 1), y)
turn (x, y) R = (cap (x + 1), y)
solution1 = scanl (foldl turn) (1, 1)
turn2 pos dir =
  case List.lookup pos lock2 of
    Nothing -> error $ show (pos, dir)
    Just more -> fromMaybe pos (List.lookup dir more)
solution2 =
  scanl (foldl turn2) 5
main = do
  print (solution1 example)
  print (solution1 input)
  print (solution2 example)
  print (solution2 input)
