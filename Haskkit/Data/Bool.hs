-- | Extensions for Data.Bool libraries.
module Haskkit.Data.Bool (
      (==>)
    ) where

-- | @a ==> b@ is 'True' whenever @a@ implies @b@, i.e., @not a || b@.
(==>) :: Bool -> Bool -> Bool
True ==> False = False
_    ==> _     = True
