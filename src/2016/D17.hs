#!/usr/bin/env stack
-- stack --resolver lts-6.26 --install-ghc runghc
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
module D16 where
import BasePrelude hiding ((&))
import Control.Lens
import Data.Set(Set)
import Data.Map(Map)
import qualified Data.Set as Set
import qualified Data.ByteString.Char8 as Bytes
import qualified Data.Map as Map
import Data.List.Split

import "cryptonite" Crypto.Hash
import Data.ByteArray.Encoding

data Dir = U | D | L | R deriving (Show, Eq, Ord)

open c =
  c >= 'b' && c <= 'f'

charOf U = 'U'
charOf D = 'D'
charOf L = 'L'
charOf R = 'R'

md5 :: String -> String
md5 s =
  take 4 $ Bytes.unpack . convertToBase Base16 . hashWith MD5 $ Bytes.pack s

doors =
  map fst . filter snd . zip [U, D, L, R] . map open . md5

neighbors (input, x, y) =
  try L (x - 1, y) <> try R (x + 1, y) <> try U (x, y - 1) <> try D (x, y + 1)
  where
    ds = doors input
    try d (x, y) | x < 0 = []
                 | y < 0 = []
                 | y > 3 = []
                 | x > 3 = []
                 | (notElem d ds) = []
                 | otherwise = [(input <> [charOf d], x, y)]


type State = (String, Int, Int)

bfs :: Monoid a => State -> (State -> Bool) -> (Set State -> a) -> Int -> IO a
bfs init success score depth =
  rec (Set.singleton init) 0
  where
    rec frontier steps
      | (depth == steps) = pure (score frontier)
      | otherwise = do
          let ok = filter success (Set.toList frontier)
          if Set.null frontier then do
            print ">>hey"
            pure mempty
          else if length ok > 0 then do
            when (steps > 300) $ print (">>found one", steps)
            -- when (steps > 300) $ print ok
            let next = Set.fromList $ concatMap neighbors (Set.toList frontier)
            ugh <- rec next (steps + 1)
            pure (score frontier <> ugh)
          else do
            let next = Set.fromList $ concatMap neighbors (Set.toList frontier)
            ugh <- rec next (steps + 1)
            pure (score frontier <> ugh)

example :: String
example =
  "hijkl"

main = do
  print (neighbors (example, 0, 0))
  -- print (bfs (example, 0, 0) (\(_, x, y) -> x == 3 && y == 3) (const (Sum 1)) 10)
  bfs ("ihgpwlah", 0, 0) (\(i, x, y) -> x == 3 && y == 3) (const (Sum 1)) 371
  print (neighbors ("ihgpwlah", 0, 0))
