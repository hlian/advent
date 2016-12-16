#!/usr/bin/env stack
-- stack --resolver lts-6.26 --install-ghc runghc --package text --package base-prelude --package split

{-# LANGUAGE NoImplicitPrelude #-}

module D3 where

import           BasePrelude
import           D3Input
import qualified Data.List as List
import           Data.List.Split

isValid =
  (== 6) . length . filter (\[a, b, c] -> a + b > c) . List.permutations
solution1 input =
  length (filter isValid input)
transpose3x3 :: [[a]] -> [[a]]
transpose3x3 =
  join . map List.transpose . chunksOf 3
solution2 input =
  length (filter isValid (transpose3x3 input))
main = do
  print (solution1 input)
  print (solution2 example2)
  print (solution2 input)
