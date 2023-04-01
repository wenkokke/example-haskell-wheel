{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -Wall #-}

module Fib where

import Data.Bits
import Data.List
import Foreign.C.Types (CInt (..))

-- Taken from:
-- https://wiki.haskell.org/The_Fibonacci_sequence#Fastest_Fib_in_the_West
fib :: Int -> Integer
fib n =
  snd . foldl' fib_ (1, 0) . dropWhile not $
    [testBit n k | k <- let s = bitSize n in [s - 1, s - 2 .. 0]]
  where
    fib_ (f, g) p
      | p = (f * (f + 2 * g), ss)
      | otherwise = (ss, g * (2 * f - g))
      where
        ss = f * f + g * g

hs_fib :: CInt -> CInt
hs_fib = fromIntegral . fib . fromIntegral

foreign export ccall hs_fib :: CInt -> CInt
