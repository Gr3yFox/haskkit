-- | Extensions for Data.Function libraries.
module Haskkit.Data.Function (
      ($$)
    ) where

infixr 0 $$
-- | Application operator on pairs, i.e., @f '$$' (x, y) == (f x, f y)@.
-- '$$' has low, right-associative binding precedence.
($$) :: (a -> b) -> (a, a) -> (b, b)
f $$ (a,b) = (f a, f b)
