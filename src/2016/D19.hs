#!/usr/bin/env stack
-- stack --resolver lts-6.26 --install-ghc runghc --package lens --package base-prelude
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ViewPatterns      #-}
module D19 where
import           BasePrelude hiding ((&), left)
import           Data.Sequence (Seq, (!?))
import qualified Data.Sequence as Seq

left n i =
  mod (i + 1) n
across n i =
  mod (i + div n 2) n

solution strategy n =
  fmap snd . find ((== -1) . fst) $ iterate (uncurry f) (0, Seq.fromList [0  ..  n-1])
  where
    f (-1) x = (-1, x)
    f i old@(Seq.length -> n)
      | n == 1 = (-1, old)
      | otherwise = do
          let victim = strategy n i
          let new = Seq.deleteAt victim old
          (left (n - 1) (i - (if victim < i then 1 else 0)), new)

main =
  mapM_ print [ solution left 5
              , solution across 5
              , solution left 3001330
              , solution across 3001330]
