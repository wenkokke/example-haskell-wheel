{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -Wall #-}

module Fib where

import Foreign.C.Types (CInt (..))

-- Taken from:
-- https://wiki.haskell.org/The_Fibonacci_sequence#Fastest_Fib_in_the_West
fib :: Int -> Int
fib n = snd . foldl fib_ (1, 0) . map (toEnum . fromIntegral) $ unfoldl divs n
  where
    unfoldl :: (Int -> Maybe (Int, Int)) -> Int -> [Int]
    unfoldl f x = case f x of
      Nothing -> []
      Just (u, v) -> unfoldl f v ++ [u]

    divs :: Int -> Maybe (Int, Int)
    divs 0 = Nothing
    divs k = Just (uncurry (flip (,)) (k `divMod` 2))

    fib_ :: (Int, Int) -> Bool -> (Int, Int)
    fib_ (f, g) p
      | p = (f * (f + 2 * g), f ^ 2 + g ^ 2)
      | otherwise = (f ^ 2 + g ^ 2, g * (2 * f - g))

hs_fib :: CInt -> CInt
hs_fib = fromIntegral . fib . fromIntegral

foreign export ccall hs_fib :: CInt -> CInt
