#!/usr/bin/env stack
-- stack --resolver lts-6.26 --install-ghc runghc
{-# LANGUAGE NoImplicitPrelude #-}
module D15 where
import BasePrelude

main =
  mapM_ (print . head . solutions) [example, input, input2]
  where
    pos (_, n, m) t =
      mod (m + t) n
    good input t0 =
      all (\(dt, disc) -> pos disc (t0 + dt) == 0) $ zip [1..] input
    solutions xs =
      [(i, b) | i <- [0..], let b = good xs i, b]

input = [(1, 17, 5), (2, 19, 8), (3, 7, 1), (4, 13, 7), (5, 5, 1), (6, 3, 0)]
input2 = input <> [(7, 11, 0)]
example = [(1, 5, 4), (2, 2, 1)]
