{-# OPTIONS_GHC -Wall                 #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Fib where

import Foreign.C.Types ( CInt(..) )

foreign export ccall hs_fib :: CInt -> CInt

hs_fib :: CInt -> CInt
hs_fib = fromIntegral . fib . fromIntegral

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)
