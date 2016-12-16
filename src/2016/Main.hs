#!/usr/bin/env stack
-- stack --resolver lts-6.26 --install-ghc runghc --package lens --package base-prelude --package bytestring --package cryptonite --package base16-bytestring --package transformers --package mtl
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where
import qualified Data.ByteString.Char8 as Bytes
import Data.ByteArray.Encoding
import           BasePrelude hiding ((&))
import qualified Data.Vector  as Vector
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Map as Map
import Data.Map (Map)
import Control.Lens
import Control.Monad.State.Strict

import "cryptonite" Crypto.Hash

limit = 64
part1 = "ihaygndm"

md5 :: String -> Int -> String
md5 door i =
  Bytes.unpack $ erg $ Bytes.pack (door <> show i)

erg :: Bytes.ByteString -> Bytes.ByteString
erg = convertToBase Base16 . hashWith MD5

md5x :: String -> Int -> String
md5x door i =
  Bytes.unpack $ iterate erg (Bytes.pack (door <> show i)) !! 2017

good :: String -> [(Int, Char, String)]
good door =
  [(i, c, h) | i <- [0..], let h = md5 door i, (c, b) <- [special h], b]

wood :: String -> [(Int, Char, String)]
wood door =
  [(i, c, h) | i <- [0..], let h = md5x door i, (c, b) <- [special h], b]

special :: String -> (Char, Bool)
special [a, b] =
  ('z', False)
special (a:b:c:rest) =
  if a == b && b == c then
    (a, True)
  else
    special (b:c:rest)

special5 :: String -> [Char]
special5 [a, b, c, d] =
  []
special5 (a:b:c:d:e:rest) =
  if a == b && b == c && c == d && d == e then
    a:(special5 (b:c:d:e:rest))
  else
    special5 (b:c:d:e:rest)

type Atom = (Int, Char, String)
type Brain = Map Int Char

f :: Atom -> State Brain [(Int, Int, Char)]
f atom@(ix, c, s) = do
   st <- get
   modify (at ix .~ Just c)
   let b = [(i,ix, y)| y <- nub (special5 s)
                              , i <- [max (ix - 1000) 0 .. max (ix - 1) 0]
                              , st ^. at i == Just y]
   pure b

go :: String -> IO ()
go input = do
  print $ (take 3 $ good input)
  print $ md5 input 816
  let x = runStateT (mapM f (take 8200 $ good input)) Map.empty
  print "h-----"
  let y = sort.concat.filter (not.null).fst $ runIdentity x
  print (length y)
  print (y !! 63)

ugh :: String -> IO ()
ugh input = do
  print $ (take 5 $ wood input)
  print $ md5x input 0
  print $ md5x input 10
  print $ md5x input 89
  print $ md5x input 25
  let x = runStateT (mapM f (take 3000 $ wood input)) Map.empty
  print "h-----"
  let y = sort.concat.filter (not.null).fst $ runIdentity x
  print y
  print (y !! 63)

main =  do
  -- go "ihaygndm"
  ugh "ihaygndm"
  -- forM (zip [0..] input) $ \(j, (i, b, c)) -> do
  --   print (j, i, b, md5x "abc" i, md5x "abc" b, c, special5 (md5x "abc" b))

