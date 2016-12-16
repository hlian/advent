#!/usr/bin/env stack
-- stack --resolver lts-6.26 --install-ghc runghc --package base-prelude --package monad-loops
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}

module D10 where
import qualified Data.Map as Map
import BasePrelude hiding (try, (&))
import Control.Monad.Loops (unfoldrM)
import Control.Lens
import Data.Map (Map)

import D10Input

domino needle code =
  let rec start chips =
              case (chips ^. at start, code ^. at start) of
                (Just bins@(sort -> [chip1, chip2]), Just (LoHi lo hi)) -> do
                  when (bins == needle) . print $ ("needle in your heart", Bot start)
                  pure chips >>= put chip1 lo >>= put chip2 hi
                -- Zero or one chips found, which is great, so do nothing.
                _ -> pure chips
              where
                put i bin@(Out out) chips =
                  print ("outputting", i, "to", bin) $> chips
                put i (Bot bot) chips =
                  chips & at bot . non [] <>~ [i]
                        & at start . non [] %~ filter (/= start)
                        & rec bot
  in rec

main = do
  domino [2, 5] exampleCode 2 exampleValues >>= print
  domino [17, 61] code 185 values >>= print
