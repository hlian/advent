{-# LANGUAGE OverloadedLists #-}
module D13Input where

import qualified Data.Vector  as Vector

data Element =
  I !Int | R !String

data Command =
  Cpy Element Element | Inc Element | Dec Element | Jnz Element Element

input :: Vector.Vector Command
input =
  [ Cpy (I 1) (R "a")
  , Cpy (I 1) (R "b")
  , Cpy (I 26) (R "d")
  , Jnz (R "c") (I 2)
  , Jnz (I 1) (I 5)
  , Cpy (I 7) (R "c")
  , Inc (R "d")
  , Dec (R "c")
  , Jnz (R "c") (I (0-2))
  , Cpy (R "a") (R "c")
  , Inc (R "a")
  , Dec (R "b")
  , Jnz (R "b") (I (-2))
  , Cpy (R "c") (R "b")
  , Dec (R "d")
  , Jnz (R "d") (I (-6))
  , Cpy (I 16) (R "c")
  , Cpy (I 17) (R "d")
  , Inc (R "a")
  , Dec (R "d")
  , Jnz (R "d") (I (-2))
  , Dec (R "c")
  , Jnz (R "c") (I (-5))
  ]

example :: Vector.Vector Command
example =
  [ Cpy (I 41) (R "a")
  , Inc (R "a")
  , Inc (R "a")
  , Dec (R "a")
  , Jnz (R "a") (I 2)
  , Dec (R "a")
  ]
