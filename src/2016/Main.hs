#!/usr/bin/env stack
-- stack runghc
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
module Main where

import           BasePrelude hiding ((&), left, lookup, loop)
import           Control.Lens
import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import           Data.Vector (Vector)
import qualified Data.Vector as Vector

import           D25Input

type State = (Int, Map Char Int, Vector Op, Int)
empty0 :: Int -> State
empty0 i = (0, Map.empty & at 'a' ?~ i, input, 0)

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
interp (PLUS a (R b)) st =
  st & _1 +~ 1
     & _2 . at b . _Just +~ lookup a st
interp (MULT (R a) b c) st =
  st & _1 +~ 1
     & _2 . at a . _Just .~ lookup b st * lookup c st
interp (OUT (R c)) st =
  seq (unsafePerformIO (putStr . show $ lookup (R c) st)) $ st & _4 +~ 1 & _1 +~ 1

loop st@(i, env, cmds, n)
  | (Vector.null cmds || i >= length cmds || n > 10) = st
  | otherwise = loop (interp (cmds ^?! ix i) st)

main = do
  -- print (loop (empty0 0))
  forM_ [0..] $ \i -> do
    print $ "here we go: " <> show i
    print (seq (loop (empty0 i)) ())
    putStr "\n"
