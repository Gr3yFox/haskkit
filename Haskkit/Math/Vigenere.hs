-- | A toy implementation of the Vigènere cipher.
-- It encrypts/decrypts only ASCII letters (while keeping the letter-case), and
-- ignores any other character. This holds both for the key and the text.
module Haskkit.Math.Vigenere (
      Operation(..)
    , vigenere
    , caesar
    ) where

import Data.List (cycle, elemIndex)
import Data.Char (ord, chr, isAscii, isAlpha)
import Haskkit.Data.Char

data Operation = Encode | Decode deriving (Show, Eq)

getOp :: Operation -> (Int -> Int -> Int)
getOp Encode = (+)
getOp Decode = (-)

encode :: Operation -> Char -> Char -> Char
encode op k c = let iR = getOp op (alphaIndex c) (alphaIndex k)
                in toCase c (chr (mod iR (intervalLength 'a' 'z') + ord 'a'))

loopKey :: String -> String
loopKey = cycle . filter isAsciiAlpha 

vigenere :: String -> Operation -> String -> String
vigenere key op text = go (loopKey key) text
    where go _ [] = []
          go (k:ks) (x:xs) | isAsciiAlpha x = encode op k x : go ks xs
                           | otherwise      = x : go (k:ks) xs

caesar :: Char -> Operation -> String -> String
caesar k op = map (encode' op k)
    where encode' op k c = if isAsciiAlpha c then encode op k c else c
