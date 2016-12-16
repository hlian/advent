{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}

module D11Input where

import qualified Data.Map as Map
import Data.Map (Map)
import Control.Lens
import Control.Lens.TH
import qualified Data.Vector  as Vector

data Furniture a =
  G !a | M !a
  deriving (Show, Eq, Ord)
makePrisms ''Furniture

data Element =
  Sr | Pu | Th | Ru | Ci | Li | H
  deriving (Show, Eq, Ord)

type State =
  (Int, Vector.Vector [Furniture Element])

input :: State
input =
  (0, [ [G Sr, M Sr, G Pu, M Pu]
      , [G Th, G Ru, M Ru, G Ci, M Ci]
      , [M Th]
      , []])

example :: State
example =
  (0, [ [M H, M Li]
      , [G H]
      , [G Li]
      , []])

me2 :: State
me2 =
  (3, [ []
      , []
      , [G H, M H]
      , [M Li, G Li]])

inputCount :: Int
inputCount =
  10

exampleCount :: Int
exampleCount =
  4

goodFloor :: [Furniture Element]
goodFloor =
  [G H, M H, G Li]
