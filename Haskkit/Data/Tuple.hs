-- | Extensions for Data.Tuple libraries.
module Haskkit.Data.Tuple (
      fst3
    , snd3
    , trd3
    ) where

-- | Extract the first component of a triple.
fst3 :: (a, b, c) -> a
fst3 (x,_,_) = x

-- | Extract the second component of a triple.
snd3 :: (a, b, c) -> b
snd3 (_,x,_) = x

-- | Extract the third component of a triple.
trd3 :: (a, b, c) -> c
trd3 (_,_,x) = x
