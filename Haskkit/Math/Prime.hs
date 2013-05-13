-- | Mathematical functions about prime numbers.
module Haskkit.Math.Prime (
      primes
    , isPrime
    ) where

import Haskkit.Data.List (elemWithin)

-- | The (infinite) list of all prime numbers.
primes :: [Int]
primes = sieve [2..]
    where
        sieve :: [Int] -> [Int]
        sieve (p:xs) = p : sieve [ x | x <- xs, x `mod` p /= 0]

-- | Tells whether a integer number is prime.
isPrime :: Int -> Bool
isPrime n = elemWithin (<=n) primes
