#!/usr/bin/env stack
-- stack --resolver lts-6.26 --install-ghc runghc --package base-prelude --package lens
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE ViewPatterns      #-}
module D12 where

import qualified Data.Vector as Vector
import qualified Data.Map as Map
import           Data.Map (Map)
import           BasePrelude hiding ((&), lookup, empty, loop)
import           Control.Lens

import           D12Input

type State = (Int, Map String Int)
empty0 = (0, Map.empty)
empty1 = empty0 & _2 . at "c" ?~ 1

lookup :: Element -> State -> Int
lookup (R r) st =  st ^. _2 . at r . non 0
lookup (I i) _ = i

interp :: Command -> State -> State
interp (Cpy src (R r)) st =
  st & _2 . at r ?~ lookup src st
     & _1 +~ 1
interp (Inc (R r)) st =
  st & _2 . at r . _Just +~ 1
     & _1 +~ 1
interp (Dec (R r)) st =
  st & _2 . at r . _Just -~ 1
     & _1 +~ 1
interp (Jnz nonzero away) st = do
  let nonzero0 = lookup nonzero st
  let away0 = lookup away st
  st & _1 %~ (\x -> x + if nonzero0 == 0 then 1 else away0)

loop st@(i, env) cmds
  | (Vector.null cmds || i >= length cmds) = st
  | otherwise = loop (interp (cmds ^?! ix i) st) cmds

main = do
  print (loop empty0 example)
  print (loop empty0 input)
  print (loop empty1 example)
  print (loop empty1 input)
