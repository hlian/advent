#!/usr/bin/env stack
-- stack --resolver lts-6.26 --install-ghc runghc --package base-prelude
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ViewPatterns #-}

module D7 where
import BasePrelude
import D7Input

isABBA :: String -> Bool
isABBA = any (\x -> length x == 4 && good x)
       . map (take 4)
       . tails
  where good [a, b, c, d] = (a == d && b == c && b /= a)

abasOf = concatMap (\x -> guard (length x == 3) >> good x)
       . map (take 3)
       . tails
  where
    good k@[a, b, c] =
      if a == c && a /= b then [k] else []

solution1 = filter $ \blocks ->
  all (not . isABBA) [s | H s <- blocks] && any isABBA [s | S s <- blocks]

solution2 = filter $ \blocks -> do
  let hypers = [s | H s <- blocks]
  or [ any (isInfixOf [b, a, b]) hypers
     | S s <- blocks, [a, b, _] <- abasOf s]

main = do
  print (solution1 [ [S "abba", H "mnop", S "qrst"]
                   , [S "abcd", H "bddb", S "xyys"]
                   , [S "aaaa", H "qwer", S "tyui"]
                   , [S "ioxxoj", H "asdfgh", S "zxcvbn"]
                   , [S "ioxxoj", H "asddsgh", S "zxcvbn"]])
  print (length $ solution1 input)
  print (solution2 [[S "aba", H "bab", S "xyz"]
                   ,[S "xyx", H "xyx", S "xyx"]
                   ,[S "aaa", H "kek", S "eke"]
                   ,[S "zazbz", H "bzb", S "cdb"]
                   ])
  print (length $ solution2 input)
