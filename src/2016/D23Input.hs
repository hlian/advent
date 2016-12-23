module D23Input where

import           Data.Vector (Vector)
import qualified Data.Vector as Vector

data Param = R Char | I Int deriving (Show, Eq)
data Op = CPY Param Param | DEC Param | INC Param | JNZ Param Param | TGL Param | MULT Param Param Param deriving (Show, Eq)

input :: Vector Op
input = [
    CPY (R 'a') (R 'b'),
    DEC (R 'b'),
    CPY (R 'a') (R 'd'),
    CPY (I 0) (R 'a'),
    MULT (R 'a') (R 'b') (R 'd'),
    CPY (I 0) (R 'c'),
    CPY (I 0) (R 'd'),
    CPY (I 0) (R 'd'),
    CPY (I 0) (R 'd'),
    CPY (I 0) (R 'd'),
    DEC (R 'b'),
    CPY (R 'b') (R 'c'),
    CPY (R 'c') (R 'd'),
    DEC (R 'd'),
    INC (R 'c'),
    JNZ (R 'd') (I(-2)),
    TGL (R 'c'),
    CPY (I(-16)) (R 'c'),
    JNZ (I 1) (R 'c'),
    CPY (I 81) (R 'c'),
    JNZ (I 73) (R 'd'),
    INC (R 'a'),
    INC (R 'd'),
    JNZ (R 'd') (I(-2)),
    INC (R 'c'),
    JNZ (R 'c') (I(-5))
  ]
