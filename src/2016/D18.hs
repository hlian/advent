#!/usr/bin/env stack
-- stack --resolver lts-6.26 --install-ghc runghc
{-# LANGUAGE NoImplicitPrelude #-}
module D15 where
import BasePrelude
import Data.List.Split

data Tile = Safe | Trap deriving (Show, Eq)

windows row =
  take (length row) (map (take 3) (tails ([Safe] <> row <> [Safe])))

become [Trap, Trap, Safe] = Trap
become [Safe, Trap, Trap] = Trap
become [Trap, Safe, Safe] = Trap
become [Safe, Safe, Trap] = Trap
become _ = Safe

next row =
  map become (windows row)

main = do
  print (windows [Trap, Trap, Trap, Trap])
  print (length $filter (==Safe) $ concat $ take 400000 $ iterate next input)

input =
    [Trap,Trap,Trap,Trap,Safe,Safe,Safe,Safe,Safe,Safe,Trap,Safe,Safe,Safe,Trap,Safe,Safe,Trap,Safe,Safe,Safe,Safe,Trap,Trap,Trap,Safe,Trap,Trap,Trap,Safe,Trap,Safe,Trap,Trap,Trap,Trap,Trap,Trap,Safe,Safe,Trap,Safe,Safe,Safe,Trap,Trap,Safe,Safe,Safe,Trap,Trap,Trap,Safe,Trap,Trap,Safe,Safe,Safe,Safe,Trap,Safe,Safe,Trap,Trap,Trap,Safe,Trap,Safe,Trap,Trap,Safe,Safe,Safe,Trap,Safe,Trap,Safe,Safe,Safe,Trap,Trap,Safe,Trap,Trap,Trap,Safe,Trap,Trap,Trap,Trap,Safe,Trap,Trap,Safe,Trap,Safe,Safe,Trap,Safe,Trap]
