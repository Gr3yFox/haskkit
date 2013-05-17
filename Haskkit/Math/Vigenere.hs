-- | A toy implementation of the Vigenere cipher.
-- It encrypts/decrypts only ASCII letters (while keeping the letter-case), and
-- ignores any other character. This holds both for the key and the text.
module Haskkit.Math.Vigenere (
      Operation(..)
    , vigenere
    , caesar
    ) where

import Data.List (cycle)
import Data.Char (ord, chr)
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

-- | Takes in input a secret key, an 'Operation', and a (possibly encrypted) text and
-- applies the Vigenere cipher.
-- Only the letters in the ASCII range from the secret key are actually used.
-- All other characters, both from the key and from the text, are ignored, as well as
-- the letter case.
vigenere :: String -> Operation -> String -> String
vigenere key op text = go (loopKey key) text
    where go _ [] = []
          go (k:ks) (x:xs) | isAsciiAlpha x = encode op k x : go ks xs
                           | otherwise      = x : go (k:ks) xs

-- | Takes in input a secret key, an 'Operation', and a (possibly encrypted) text and
-- applies the Caesar cipher.
-- Only letters in the ASCII range from are valid keys.
caesar :: Char -> Operation -> String -> String
caesar k op = map (encode' op k)
    where encode' op k c = if isAsciiAlpha c then encode op k c else c
