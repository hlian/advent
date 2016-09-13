module Vigenere where

import BasePrelude
import qualified Data.Map  as M
import qualified Data.List as L

type Crypt a = a -> a -> CipherEnv -> Maybe a
type CharSet = [Char] -- alias for string, used to distinguish from text input
type CipherTable = M.Map Char CharSet

data CipherEnv = Cipher { cipherChars :: CharSet
                        , cipherTable :: CipherTable
                        } deriving (Show)

mkTable :: CharSet -> CipherTable
mkTable charset =
  M.fromList (zip charset (map f charset))
  where
    f char = drop (fromJust $ L.elemIndex char charset) (cycle charset)

-- generates a cryptographic key based on a message and a keyword
-- if the message is "hello there", and the keyword is "apple";
-- then the resulting key will be "appleapplea"
genKey :: String -> String -> String
genKey msg kw = kw `padTo` length msg
  where padTo xs size = take size $ cycle xs

-- gets a row from the table at a specific key k
-- if no row exists, Nothing is returned
getRow ::  Char -> CipherEnv -> Maybe String
getRow k e = M.lookup k $ cipherTable e

-- gets the vth letter on the kth row on the table
-- due to the fact that both getRow and baseIndex are Maybe types,
-- Applicative mapping needs to be used
encryptLetter :: Crypt Char -- table[k, base[v]]
encryptLetter k v e = (!!) <$> getRow k e <*> baseIndex
  where baseIndex = L.elemIndex v base -- gets index from base
        base = cipherChars e

-- gets the index of the cth letter on the kth row in the table,
-- then uses that index to find the cth letter in the base
decryptLetter :: Crypt Char -- base[table[k, c]]
decryptLetter k c e = (base !!) <$> tableIndex
  where tableIndex = getRow k e >>= L.elemIndex c -- gets index from table
        base = cipherChars e

-- maps a function to two strings, making sure both strings are up-cased
-- mapM is used because f returns a monad,
-- and neither of the strings are monads themselves
crypt :: Crypt Char -> Crypt String
crypt f w kw e = mapM (uncurry3 f) zipped
  where word = upCase w
        keyword = upCase kw
        zipped = zip3 key word $ repeat e
        key = genKey word keyword

-- encrypts the string using a message and a keyword
encrypt :: Crypt String
encrypt = crypt encryptLetter

-- decrypts using a message and a keyword
decrypt :: Crypt String
decrypt = crypt decryptLetter

-- helper function to turn a string into upper-case
upCase :: String -> String
upCase = map toUpper

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f = \(a, b, c) -> f a b c

mkEnv :: CharSet -> CipherEnv
mkEnv c = Cipher c $ mkTable c

runEnv :: CharSet -> Crypt String -> String -> String -> Maybe String
runEnv c f a b =  f a b $ mkEnv c

runFun :: CharSet -> (CipherEnv -> Maybe String) -> Maybe String
runFun c f = f $ mkEnv c
