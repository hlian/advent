{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where
import BasePrelude hiding ((&), cast)
import Control.Lens
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Vector (Vector)
import qualified Data.Vector as Vector

newtype I = I [String] deriving (Show, Eq)
data State = State { _regs :: Map String Int, _is :: Vector I } deriving (Show, Eq)
makeLenses ''State

state0 = Map.fromList [("a", 1)]

-- even i = case divMod i 2 of (_, 0) -> True; _ -> False

step :: State -> [String] -> (State, Int)
step st ["hlf", r] = (st & regs . at r %~ (Just . (`div` 2) . fromMaybe 0), 1)
step st ["tpl", r] = (st & regs . at r %~ (Just . (* 3) . fromMaybe 0), 1)
step st ["inc", r] = (st & regs . at r %~ (Just . (+ 1) . fromMaybe 0), 1)
step st ["jmp", o] = (st, fromMaybe (error "o") $ readMaybe o)
step st ["jie", r, o] = if even (fromMaybe 0 (st ^. regs . at r)) then (st, fromMaybe (error "o") $ readMaybe o) else (st, 1)
step st ["jio", r, o] = if Just 1 == (st ^. regs . at r) then (st, fromMaybe (error "o") $ readMaybe o) else (st, 1)

iterateM :: (a -> Maybe a) -> a -> [a]
iterateM f x = x:(case f x of Nothing -> []; Just j -> iterateM f j)

run :: (State, Int) -> Maybe (State, Int)
run (st, pc) = do
  I i <- (st ^? is . ix pc)
  return ((step st i) & _2 %~ (+ pc))

input :: IO State
input = (State state0 . Vector.fromList . map (I . words) . lines . filter (/= '+') . filter (/= ',')) <$> readFile "/Users/hao/lab/advent/23.txt"

