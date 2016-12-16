#!/usr/bin/env stack
-- stack --resolver lts-6.26 --install-ghc runghc --package lens --package base-prelude --package bytestring --package cryptonite --package base16-bytestring --package mtl
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}
module Main where
import              BasePrelude
import qualified    Data.ByteString.Char8 as Bytes
import qualified    Data.Map as Map

import "cryptonite" Crypto.Hash
import              Control.Lens
import              Control.Monad.State.Strict
import              Data.ByteArray.Encoding

type Atom = (Int, Char, String)
type Brain = Map.Map Int Char

input = "ihaygndm"

md5 strength door i =
  Bytes.unpack $ iterate erg (Bytes.pack (door <> show i)) !! (strength + 1)
  where erg = convertToBase Base16 . hashWith MD5

triples :: Int -> String -> [Atom]
triples strength door =
  [(i, c, h) | i <- [0..]
             , let h = md5 strength door i
             , Just c <- [listToMaybe (repeats 3 h)]]

repeats len =
  map head . filter identical . filter full . map (take len) . tails
  where identical = (== 1) . length . nub
        full = (== len) . length

solve strength input = print (keys !! 63)
  where
    keys =
      keysOf . take 3000 $ triples strength input
    keysOf =
      sort . concat . (`evalState` Map.empty) . mapM f
    f atom@(curr, c, hash) = do
      brain <- get
      at curr .= Just c
      pure [ (prev, curr, x)
           | x <- nub (repeats 5 hash)
           , prev <- [max (curr - 1000) 0 .. max (curr - 1) 0]
           , brain ^. at prev == Just x]

main =  do
  solve 0 "abc"
  solve 0 "ihaygndm"
  solve 2016 "abc"
  solve 2016 "ihaygndm"
