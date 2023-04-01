{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -Wall #-}

module Fib where

import Control.Monad (forM_)
import Foreign.Ptr (Ptr)
import Foreign.C.Types (CInt (..))
import Foreign.C.String (CString, peekCString)
import Foreign.Marshal.Array (peekArray)
import Text.Read (readMaybe)

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
      | p = (f * (f + 2 * g), f `pow` 2 + g `pow` 2)
      | otherwise = (f `pow` 2 + g `pow` 2, g * (2 * f - g))

    pow :: Int -> Int -> Int
    pow = (^)

hs_fib :: CInt -> CInt
hs_fib = fromIntegral . fib . fromIntegral

foreign export ccall hs_fib :: CInt -> CInt

defaultMain :: [String] -> IO ()
defaultMain args =
  forM_ args $ \arg ->
    case readMaybe arg of
      Just n  -> putStrLn $ "fib " <> show n <> " -> " <> show (fib n)
      Nothing -> putStrLn $ "fib " <> arg    <> " -> error"

hs_defaultMain :: CInt -> Ptr CString -> IO ()
hs_defaultMain c_argc c_argv = do
  let argc = fromIntegral c_argc
  argv <- mapM peekCString =<< peekArray argc c_argv
  defaultMain argv

foreign export ccall hs_defaultMain :: CInt -> Ptr CString -> IO ()
