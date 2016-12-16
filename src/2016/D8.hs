#!/usr/bin/env stack
-- stack --resolver lts-6.26 --install-ghc runghc --package base-prelude --package mtl --package lens
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ViewPatterns #-}

module D8 where
import BasePrelude
import Control.Monad.Reader
import Control.Lens
import D8Input

coord width i = (mod i width, div i width)
indice width (x, y) = y * width + x
makeScreen =
  reader $ \(width, height) ->
    zip [0..] $ take (width * height) (repeat False)

apply screen (R a b) =
  reader $ \(width, height) ->
    [ (i, if x < a && y < b then True else pixel)
    | (i@(coord width -> (x, y)), pixel) <- screen
    ]
apply screen (RC a b) = do
  reader $ \(width, height) ->
    let rot x y = indice width (x, mod (y - b) height) in
    [ (i, if x == a then screen ^?! ix (rot x y) . _2 else pixel)
    | (i@(coord width -> (x, y)), pixel) <- screen
    ]
apply screen (RR a b) =
  reader $ \(width, height) ->
    let rot x y = indice width (mod (x - b) width, y) in
    [ (i, if y == a then screen ^?! ix (rot x y) . _2 else pixel)
    | (i@(coord width -> (x, y)), pixel) <- screen
    ]

main = do
  runReaderT (run example) (7, 3)
  runReaderT (run input) (50, 6)
  where
    run :: [Command] -> ReaderT (Int, Int) IO ()
    run input = do
      width <- view _1
      screen0 <- makeScreen
      screenf <- foldlM apply screen0 input
      liftIO $ display width screenf
    display width screen = do
      print (length $ filter snd screen)
      forM_ screen $ \(i, pixel) -> do
        let (x, y) = coord width i
        putChar $ if pixel then '#' else '.'
        when (x == width - 1) (putChar '\n')
