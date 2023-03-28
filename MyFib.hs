{-# OPTIONS_GHC -Wall                 #-}

{-# LANGUAGE ForeignFunctionInterface #-}

module MyFib (fib) where

import Foreign.C.Types

fib :: Int -> Integer
fib n = fibs !! n
  where
    fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

fib_hs :: CInt -> CInt
fib_hs = fromIntegral . fib . fromIntegral

foreign export ccall fib_hs :: CInt -> CInt
