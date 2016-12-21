#!/usr/bin/env stack
-- stack --resolver lts-7.13 --install-ghc runghc --package hxt --package HandsomeSoup --package hxt-xpath --package base-prelude
{-# LANGUAGE Arrows #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Wow where

import Text.HandsomeSoup
import Text.XML.HXT.Core

data Rock = Rock String String String deriving Show

atTag tag =
  deep (isElem >>> hasName tag)

text =
  deep isText >>> getText

getR =
  atTag "tbody" //> atTag "tr"
  >>> proc x -> do
        rowID <- x >- getAttrValue "id"
        name <- x >- atTag "td" >. (!! 0) >>> text
        kind <- x >- atTag "td" >. (!! 1) >>> text
        returnA -< Rock rowID name kind

main = do
  dt <- readFile "html.html"
  result <- runX $ parseHtml dt //> hasAttrValue "id" (== "Greatest-Table") >>> getR
  print result
