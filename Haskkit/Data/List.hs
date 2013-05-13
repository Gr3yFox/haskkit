-- | Extensions for Data.List libraries.
module Haskkit.Data.List (
      replace
    , deleteBy'
    , elemWithin
    ) where

import Data.List (find)

-- | Applied to two elements and a list, 'replace' changes all occurrences
-- of the first element into occurrences of the second.
-- 
-- > replace 'k' 'l' "hekko" == "hello"
replace :: (Eq a) => a -> a -> [a] -> [a]
replace x x' = map (\y -> if y == x then x' else y)

-- | Applied to a predicate and a list, deletes the first element satisfying
-- the predicate and returns the deleted element (if any) together with the
-- resulting list.
-- 
-- > deleteBy' (>4) [0,1,2,3] == (Nothing,[0,1,2,3])
-- > deleteBy' (>1) [0,1,2,3] == (Just 2,[0,1,3])
deleteBy' :: (a -> Bool) -> [a] -> (Maybe a, [a])
deleteBy' pred xs = let (l, r) = break pred xs
                    in case r of
                         [] -> (Nothing, l)
                         _  -> (Just $ head r, l ++ tail r)

-- | For a predicate @p@, @'elemWithin' p x ys@ is 'True' if and only if @x@
-- precedes the first element satisfying @p@ in the list @ys@.
-- 'elemWithin' is useful, e.g., for searching elements into infinite ordered
-- lists, by using the predicate as a termination criterion.
--
-- > elemWithin (<=10) 10 [1..] == True
-- > elemWithin (<10) 10 [1..] == False
-- > elemWithin (<=10) (-1) [1..] == False
-- > elemWithin (<=15) 15 [2,4..] == False
elemWithin :: (Eq a) => (a -> Bool) -> a -> [a] -> Bool
elemWithin p x = go
    where
        go [] = False
        go (y:ys)
            | p y       = x == y || go ys
            | otherwise = False