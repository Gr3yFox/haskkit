{-# LANGUAGE RankNTypes #-}

-- | This module contains the Church encodings for booleans and natural numbers.
-- See http://en.wikipedia.org/wiki/Church_encoding for reference.
module Haskkit.Data.Church (
      CBool
    , ctrue
    , cfalse
    , bool2church
    , church2bool
    , cand
    , cor
    , cnot
    , cif
    , CNat
    , czero
    , csucc
    , csum
    , church2nat
    , nat2church2
    , ciszero
    ) where

import Data.Function (fix)



-- | Church's booleans.
type CBool = forall a. a -> a -> a

-- | Church's equivalent of 'True'.
ctrue :: CBool
ctrue  t f = t

cfalse :: CBool
-- | Church's equivalent of 'False'
cfalse t f = f

-- | Converts a 'Bool' into a 'CBool'.
bool2church :: Bool -> CBool
bool2church True = ctrue
bool2church False = cfalse

-- | Covnerts a 'CBool' into a 'Bool'
church2bool :: CBool -> Bool
church2bool c = c True False

-- | Conjunction between 'CBool'.
cand :: CBool -> CBool -> CBool
cand l r = l r l

-- | Disjunction between 'CBool'.
cor :: CBool -> CBool -> CBool
cor  l r = l l r

-- | Negation of a 'CBool'.
cnot :: CBool -> CBool
cnot b = b cfalse ctrue

--  | Conditional if-then-else for 'CBool'.
-- @'cif' b thn els@ is equal to @thn@ when @b@ is 'ctrue', and equal to @els@ when @b@ is 'cfalse'.
cif :: CBool -> a -> a -> a
cif cb = cb

-- CNat: Church's natural numbers

-- | Church's natural numbers.
type CNat = forall a. (a -> a) -> a -> a

-- | Equivalent of 0.
czero :: CNat
czero _ x = x

-- | Successor function.
csucc :: CNat -> CNat
csucc cn f x = f (cn f x)

-- | Sum between 'CNat'.
csum :: CNat -> CNat -> CNat
csum cn1 cn2 f x = cn1 f (cn2 f x)

-- | Conversion from Church's naturals to regular integers.
church2nat :: CNat -> Int
church2nat cn = cn (+1) 0

-- | Conversion from (positive) integers to 'CNat'.
nat2church :: Int -> CNat
nat2church n | n <= 0    = czero
             | otherwise = csucc (nat2church (n - 1))

-- | 'True' if and only if the given 'CNat' is equivalent to 0.
ciszero :: CNat -> CBool
ciszero cn = cn (const cfalse) ctrue

-- | Factorial function for 'CNat'.
fact :: Int -> Int
fact = fix factfun
  where
    factfun :: (Int -> Int) -> Int -> Int
    factfun f x = if x <= 0 then 1 else x * f (x - 1)

