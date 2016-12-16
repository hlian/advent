#!/usr/bin/env stack
-- stack --resolver lts-6.26 --install-ghc runghc
{-# LANGUAGE NoImplicitPrelude #-}
module D16 where
import BasePrelude

stretch input =
  take input . fromJust . find ((>= input) . length) . iterate expand
  where expand s = s <> "0" <> map boop (reverse s)
        boop '0' = '1'
        boop '1' = '0'

checksum s =
  iterate smash s !! times 0 (length s)
  where
    smash (a:b:rest) = (if a == b then "1" else "0") <> smash rest
    smash [] = []
    times i n | odd n = i
              | otherwise = times (i + 1) (div n 2)

main =
  mapM (print . checksum) [ stretch 20 "10000"
                          , stretch 272 "10010000000110000"
                          , stretch 35651584 "10010000000110000"
                          ]
