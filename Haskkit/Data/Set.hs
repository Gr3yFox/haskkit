-- | Extensions for Data.Set libraries.
module Haskkit.Data.Set (
      everything
    ) where

import Data.Set (Set)
import qualified Data.Set as S
    
-- | @'everything' p s@ returns 'True' if and only if all elements in 'Set' @s@
-- satisfy predicate @p@.
everything :: Ord a => (a -> Bool) -> Set a -> Bool
everything p = S.null . S.filter (not . p)
