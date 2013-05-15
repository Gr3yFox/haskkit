{-# LANGUAGE RankNTypes #-}

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

-- CBool: Church's booleans

type CBool = forall a. a -> a -> a

ctrue, cfalse :: CBool
ctrue  t f = t
cfalse t f = f

bool2church :: Bool -> CBool
bool2church True = ctrue
bool2church False = cfalse

church2bool :: CBool -> Bool
church2bool c = c True False

cand, cor :: CBool -> CBool -> CBool
cand l r = l r l
cor  l r = l l r

cnot :: CBool -> CBool
cnot b = b cfalse ctrue

cif :: CBool -> a -> a -> a
cif cb = cb

-- CNat: Church's natural numbers

type CNat = forall a. (a -> a) -> a -> a

czero :: CNat
czero _ x = x

csucc :: CNat -> CNat
csucc cn f x = f (cn f x)

csum :: CNat -> CNat -> CNat
csum cn1 cn2 f x = cn1 f (cn2 f x)

church2nat :: CNat -> Int
church2nat cn = cn (+1) 0

nat2church :: Int -> CNat
nat2church n | n <= 0    = czero
             | otherwise = csucc (nat2church (n - 1))

ciszero :: CNat -> CBool
ciszero cn = cn (const cfalse) ctrue

fact :: Int -> Int
fact = fix factfun
  where
    factfun :: (Int -> Int) -> Int -> Int
    factfun f x = if x <= 0 then 1 else x * f (x - 1)

