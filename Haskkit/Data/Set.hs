-- | Extensions for Data.Set libraries.
module Haskkit.Data.Set (
      all
    ) where

import Prelude hiding (all)
import Data.Set (Set)
import qualified Data.Set as S

    
-- | @'all' p s@ returns 'True' if and only if all elements in 'Set' @s@
-- satisfy predicate @p@.
all :: Ord a => (a -> Bool) -> Set a -> Bool
all p = S.null . S.filter (not . p)
