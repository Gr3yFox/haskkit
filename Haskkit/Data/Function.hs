-- | Extensions for Data.Function libraries.
module Haskkit.Data.Function (
      ($$)
    ,  is
    , (|=?)
    , isNot
    , if'
    , (?)
    ) where

infixr 0 $$
-- | Application operator on pairs, i.e., @f '$$' (x, y) == (f x, f y)@.
-- '$$' has low, right-associative binding precedence.
($$) :: (a -> b) -> (a, a) -> (b, b)
f $$ (a,b) = (f a, f b)

-- | Combinator for predicates.
-- 
-- > data D = D { prop :: Int }
-- > filter (prop `is` (>3)) [D 1, D 3, D 5, D 6] == [D 5, D 6]
is :: (a -> b) -> (b -> Bool) -> a -> Bool
is = flip (.)

-- | 'is' as an infix operator.
(|=?) :: (a -> b) -> (b -> Bool) -> a -> Bool
(|=?) = is

-- | Combinator for predicates. @f `isNot` p@ is equivalent to
-- @f `is` (not . p)@.
isNot :: (a -> b) -> (b -> Bool) -> a -> Bool
f `isNot` p = (not . p) . f

-- | A if-then-else rewritten as a function, i.e., @if' cond thn els@ is
-- equivalent to @if cond then thn else els@.
-- See http://www.haskell.org/haskellwiki/If-then-else for information and
-- usage examples.
if' :: Bool -> a -> a -> a
if' True  x _ = x
if' False _ y = y

infixr 1 ?
-- | '(?)' is the same as 'if\'', except that it is a low, right-associative
-- binding precedence.
--
-- > (cond ? thn $ els) == if cond then thn else els
(?) :: Bool -> a -> a -> a
(?) = if'
