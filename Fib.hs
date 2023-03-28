{-# OPTIONS_GHC -Wall                 #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Fib where

import Foreign.C.Types ( CInt(..) )

foreign export ccall fib_hs :: CInt -> CInt

fib_hs :: CInt -> CInt
fib_hs = fromIntegral . fib . fromIntegral

fib :: Int -> Integer
fib n = fibs !! n
  where
    fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
