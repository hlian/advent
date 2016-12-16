#!/usr/bin/env stack
-- stack --resolver lts-6.26 --install-ghc runghc --package lens --package base-prelude

{-# LANGUAGE NoImplicitPrelude #-}

module D4 where

import           BasePrelude hiding (rotate)
import           Control.Lens
import           D4Input
import qualified Data.List as List

count x =
  length . filter (== x)
counts xs =
  sortBy (comparing (_1 %~ Down))
  . filter ((/= '-') . view _2)
  . nub
  . map (flip count xs &&& id) $ xs
checksum =
  map snd . take 5 . counts

rotate _ '-' =
  ' '
rotate d c =
  chr $ start + (mod (delta + d) n)
  where
    start = ord 'a'
    delta = ord c - ord 'a'
    n = ord 'z' - start + 1

solution =
  sum . map (view _2) . filter (\(input, _, check) -> checksum input == check)
solution2 (input, d, _) =
  map (rotate d) input

main = do
  print (solution [("aaaaa-bbb-z-y-x", 123, "abxyz")])
  print (solution input)
  print (map (rotate 343) "qzmt-zixmtkozy-ivhz")
  print (filter (List.isInfixOf "north" . snd) . map (id &&& solution2) $ input)
