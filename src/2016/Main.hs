{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GADTs #-}
module Main where

import           BasePrelude hiding ((&))
import           Control.Lens
import           D24Input
import qualified Data.Map as Map
import qualified Data.Set as Set

graph input =
  Map.fromList [((x, y), c) | (y, s) <- zip [0..] input, (x, c) <- zip [0..] s]

wire needle graph =
  head [(needle, x, y) | ((x, y), c) <- Map.toList graph, c == needle]

neighbors g (_, x, y) = concat . map try $ [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]
  where try (x, y) = case g ^. at (x, y) of
          Just c | c /= '#' -> [(c, x, y)]
          _ -> []

dist init goal neighbors = rec (Set.singleton init) 0
  where rec frontier steps = do
          let ok = filter (== goal) (Set.toList frontier)
          if Set.null frontier then error "invalid goal story"
          else if length ok > 0 then steps
          else rec (Set.fromList . concatMap neighbors . Set.toList $ frontier) (steps + 1)


measure dists  =
  foldl (\(last, acc) next -> (next, (find (last, next):acc))) ('0', [])
  where
    find (x, y) = fromJust $ dists ^. at (x, y) <|> dists ^. at (y, x)

comb 0 _ = [[]]
comb _ [] = []
comb m (x:xs) = map (x:) (comb (m-1) xs) ++ comb m xs

main = do
  print . minimum $ map (sum . snd . measure dists) (permutations . drop 1 $ numbers)
  print . minimum $ map (sum . snd . measure dists . (<> "0")) (permutations . drop 1 $ numbers)
  where
    (numbers, g) = ("01234567", graph input)
    dists = Map.fromList $ [((src, dst), dist (wire src g) (wire dst g) (neighbors g))
                           | [src, dst] <- comb 2 numbers]
