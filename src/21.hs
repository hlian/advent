{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import BasePrelude
import Control.Lens

newtype Cost = Cost Int deriving (Show, Eq, Ord, Num)
newtype Damage = Damage Int deriving (Show, Eq, Ord, Num)
newtype Armor = Armor Int deriving (Show, Eq, Ord, Num)

comb :: Int -> [a] -> [[a]]
comb 0 _ = [[]]
comb _ [] = []
comb m (x:xs) = map (x:) (comb (m-1) xs) ++ comb m xs

ringSets :: [[(Cost, Damage, Armor)]]
ringSets = comb 0 xs ++ comb 1 xs ++ comb 2 xs
  where xs = [(Cost 25  , Damage 1, Armor 0)
             ,(Cost 50  , Damage 2, Armor 0)
             ,(Cost 100 , Damage 3, Armor 0)
             ,(Cost 20  , Damage 0, Armor 1)
             ,(Cost 40  , Damage 0, Armor 2)
             ,(Cost 80  , Damage 0, Armor 3)
             ]

weapons :: [(Cost, Damage)]
weapons = [(Cost 8  , Damage 4)
          ,(Cost 10 , Damage 5)
          ,(Cost 25 , Damage 6)
          ,(Cost 40 , Damage 7)
          ,(Cost 74 , Damage 8)
          ]

armors :: [(Cost, Armor)]
armors = [(Cost 13  , Armor 1)
         ,(Cost 31  , Armor 2)
         ,(Cost 53  , Armor 3)
         ,(Cost 75  , Armor 4)
         ,(Cost 102 , Armor 5)
         ,(Cost 0   , Armor 0)
         ]

space :: [(Cost, Damage, Armor)]
space = do
  (wc, wd) <- weapons
  (ac, aa) <- armors
  rings <- ringSets
  return (sum ([wc, ac] <> rings ^.. traverse . _1),
          sum ([wd] <> rings ^.. traverse . _2),
          sum ([aa] <> rings ^.. traverse . _3))

bossHP :: Double
bossHP = 109

turns :: Damage -> Int
turns (Damage d) = ceiling (bossHP / fromIntegral (d - 2))

part1 :: (Cost, Damage, Armor)
part1 = minimumBy (compare `on` (^. _1)) (filter f space)
  where f (_, d, Armor a) = (turns d - 1) * (8 - a) <= 100

part2 :: (Cost, Damage, Armor)
part2 = maximumBy (compare `on` (^. _1)) (filter f space)
  where f (_, d, Armor a) = (turns d) * (8 - a) >= 100

main :: IO ()
main = print part1 >> print part2
