{-# LANGUAGE OverloadedLists #-}

module D25Input where

import           Data.Vector (Vector)
import qualified Data.Vector as Vector

data Param = R Char | I Int deriving (Show, Eq)
data Op = CPY Param Param | DEC Param | INC Param | JNZ Param Param | TGL Param | MULT Param Param Param | PLUS Param Param | OUT Param deriving (Show, Eq)


input :: Vector Op
input = [
    CPY (R 'a') (R 'd'),
    CPY (I $ 15) (R 'c'),
    CPY (I $ 170) (R 'b'),
    INC (R 'd'),
    DEC (R 'b'),
    JNZ (R 'b') (I $ -2),
    DEC (R 'c'),
    JNZ (R 'c') (I $ -5),
    CPY (R 'd') (R 'a'),
    JNZ (I $ 0) (I $ 0),
    CPY (R 'a') (R 'b'),
    CPY (I $ 0) (R 'a'),
    CPY (I $ 2) (R 'c'),
    JNZ (R 'b') (I $ 2),
    JNZ (I $ 1) (I $ 6),
    DEC (R 'b'),
    DEC (R 'c'),
    JNZ (R 'c') (I $ -4),
    INC (R 'a'),
    JNZ (I $ 1) (I $ -7),
    CPY (I $ 2) (R 'b'),
    JNZ (R 'c') (I $ 2),
    JNZ (I $ 1) (I $ 4),
    DEC (R 'b'),
    DEC (R 'c'),
    JNZ (I $ 1) (I $ -4),
    JNZ (I $ 0) (I $ 0),
    OUT (R 'b'),
    JNZ (R 'a') (I $ -19),
    JNZ (I $ 1) (I $ -21)
    ]
