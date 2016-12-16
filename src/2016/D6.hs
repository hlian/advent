#!/usr/bin/env stack
-- stack --resolver lts-6.26 --install-ghc runghc --package base-prelude

{-# LANGUAGE NoImplicitPrelude #-}

module D6 where

import BasePrelude
import D6Input

main =
  print ( solution1 example
        , solution1 input
        , solution2 example
        , solution2 input)
  where
    solution1 input = map most (transpose input)
    solution2 input = map least (transpose input)
    most xs         = argmax (count xs) xs
    least xs        = argmax (negate . count xs) xs
    count xs x      = length . filter (== x) $ xs
    argmax f xs     = maximumBy (comparing f) xs

