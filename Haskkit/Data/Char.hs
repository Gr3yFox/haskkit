-- | Extensions for Data.Char libraries.
module Haskkit.Data.Char (
      alphaIndex
    , toCase
    , intervalLength
    , isAsciiAlpha
    ) where

import Data.Char

-- | @'alphaIndex' c@ returns the index (starting from 0) of the given letter
-- of the alphabet. Both lowercase and uppercase ASCII letters are supported.
alphaIndex :: Char -> Int
alphaIndex x = ord x - ord (if isLower x then 'a' else 'A')

-- | When the given character is lower- or upper-case, returns respectively
-- 'toLower' or 'toUpper'.
toCase :: Char -> Char -> Char
toCase sample = if isLower sample then toLower else toUpper

-- | Computes the number of characters between two given bounds, i.e.
-- @'intervalLength' min max = 'ord' max - 'ord' min + 1@.
intervalLength :: Char -> Char -> Int
intervalLength min max = ord max - ord min + 1

-- | Returns 'True' for any ASCII character @c@ which is either a lower-case or
-- upper-case letter, i.e., if @'isAscii' c && 'isAlpha' c@.
isAsciiAlpha :: Char -> Bool
isAsciiAlpha c = isAscii c && isAlpha c
