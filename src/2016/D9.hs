#!/usr/bin/env stack
-- stack --resolver lts-6.26 --install-ghc runghc --package base-prelude --package megaparsec
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}

module D9 where
import BasePrelude hiding (try)
import Text.Megaparsec
import Text.Megaparsec.String
import Text.Megaparsec.Lexer

import D9Input

command = between (char '(') (char ')') $ do
  a <- fromIntegral <$> integer
  char 'x'
  b <- fromIntegral <$> integer
  pure (a, b)

parser recurse =
  fmap sum . many $ (try command <|> pure (0, 0)) >>= \case
    (0, 0) ->
      length <$> some (satisfy (/= '('))
    (len, times) -> do
      repeated <- count len anyChar
      pure $ times * (if recurse then decompress recurse repeated else length repeated)

decompress recurse =
  fromJust . parseMaybe (parser recurse)

main = do
  print . decompress1 $ "ADVENT"
  print . decompress1 $ "A(1x5)BC"
  print . decompress1 $ "(3x3)XYZ"
  print . decompress1 $ "(6x1)(1x3)A"
  print . decompress1 $ "X(8x2)(3x3)ABCY"
  print . decompress1 $ input
  print . decompress2 $ "(25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN"
  print . decompress2 $ input
  where
    decompress1 = decompress False
    decompress2 = decompress True
