#!/usr/bin/env stack
-- stack --resolver lts-6.26 --install-ghc runghc --package lens --package base-prelude --package bytestring --package cryptonite --package base16-bytestring

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PackageImports #-}

module D5 where

import qualified Data.ByteString.Char8 as Bytes
import BasePrelude
import "cryptonite" Crypto.Hash
import Control.Lens
import Data.ByteArray.Encoding
import D5Input

md5 door i =
  convertToBase Base16 . hashWith MD5 $ Bytes.pack (door <> show i)

fiveZeroes :: Bytes.ByteString -> Bool
fiveZeroes =
  (== (Bytes.pack "00000")) . Bytes.take 5

search door =
  [ (Bytes.index md50 6, i)
  | i <- [0..]
  , let md50 = md5 door i
  , fiveZeroes md50
  ]

search2 door =
  [ (i, Bytes.index md50 5, Bytes.index md50 6)
  | i <- [0..]
  , let md50 = md5 door i
  , let six = Bytes.index md50 5
  , fiveZeroes md50
  , six >= '0' && six <= '7'
  ]

main = do
  print ((fiveZeroes &&& id) $ md5 "abc" 3231929)
  print ((fiveZeroes &&& id) $ md5 "abc" 5017308)
  print ((fiveZeroes &&& id) $ md5 "abc" 5278568)
  print (sort . take 16 $ search2 "cxdnnyjw")
