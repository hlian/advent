#!/usr/bin/env stack
-- stack runghc
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
module D23 where

import           BasePrelude hiding ((&), left, lookup, loop)
import           Control.Lens
import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import           Data.Vector (Vector)
import qualified Data.Vector as Vector

import           Data.Hashable

import           AStar
import           D23Input

type State = (Int, Map Char Int, Vector Op)
empty0 :: Int -> State
empty0 i = (0, Map.empty & at 'a' ?~ i, input)

lookup :: Param -> State -> Int
lookup (R r) st =  st ^. _2 . at r . non 0
lookup (I i) _ = i

interp :: Op -> State -> State
interp (CPY src (R r)) st =
  st & _2 . at r ?~ lookup src st
     & _1 +~ 1
interp (INC (R r)) st =
  st & _2 . at r . _Just +~ 1
     & _1 +~ 1
interp (DEC (R r)) st =
  st & _2 . at r . _Just -~ 1
     & _1 +~ 1
interp (JNZ nonzero away) st = do
  let nonzero0 = lookup nonzero st
  let away0 = lookup away st
  st & _1 %~ (\x -> x + if nonzero0 == 0 then 1 else away0)
interp (PLUS (R a) (R b)) st =
  st & _2 . at a . _Just +~ lookup (R b) st
     & _1 +~ 1
interp NOP st =
  st & _1 +~ 1
interp (MULT (R a) (R b) (R c)) st =
  st & _1 +~ 1
     & _2 . at a . _Just .~ lookup (R b) st * lookup (R c) st
interp (TGL (R c)) st =
  st & _1 +~ 1
     & _3 . ix (st ^. _1 + lookup (R c) st) %~ toggle

toggle :: Op -> Op
toggle (CPY a b) = (JNZ a b)
toggle (TGL a) = INC a
toggle (DEC a) = INC a
toggle (INC a) = DEC a
toggle (JNZ a b) = CPY a b

loop st@(i, env, cmds)
  | (Vector.null cmds || i >= length cmds) = st
  | otherwise = loop (interp (cmds ^?! ix i) st)

main = do
  print (loop (empty0 7))
  print (loop (empty0 12))
