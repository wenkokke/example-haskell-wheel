{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -Wall #-}

module ExampleHaskellWheelBinding where

import Control.Exception (Exception (..), SomeException (..), handle)
import Control.Monad (forM_)
import Data.ByteString (packCStringLen, useAsCString)
import Data.ByteString.Unsafe (unsafePackCStringFinalizer)
import Data.Typeable (Typeable)
import Data.Version (showVersion)
import Foreign.C.String (CString, newCString, withCString)
import Foreign.C.Types (CInt (..))
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Marshal.Alloc (free)
import Foreign.Ptr (castPtr, plusPtr)
import GHC.IO.Buffer (Buffer (..), newByteBuffer)
import GHC.IO.BufferedIO (BufferedIO (..))
import GHC.IO.Device (IODevice (..), IODeviceType (..), RawIO (..))
import GHC.IO.Handle (Handle, Handle__ (..), hDuplicateTo, noNewlineTranslation, withHandle_)
import GHC.IO.Handle.Internals (mkFileHandleNoFinalizer)
import GHC.IO.Handle.Types (NewlineMode (..))
import Paths_example_haskell_wheel (version)
import System.Environment (getArgs)
import System.Exit (ExitCode (..))
import System.IO (IOMode (..), stderr, stdout)
import Text.Read (readMaybe)

foreign import ccall unsafe_py_format_stdout :: CString -> IO ()

foreign import ccall unsafe_py_format_stderr :: CString -> IO ()

data PyDevice = PyStdout | PyStderr
  deriving (Show, Typeable)

unsafe_py_format :: PyDevice -> CString -> IO ()
unsafe_py_format PyStdout = unsafe_py_format_stdout
unsafe_py_format PyStderr = unsafe_py_format_stderr

instance IODevice PyDevice where
  ready _ _ _ = return True
  close _ = return ()
  devType _ = return RegularFile

instance RawIO PyDevice where
  read dev _ _ _ = error $ show dev ++ ": read not supported"
  readNonBlocking dev _ _ _ = error $ show dev ++ ": readNonBlocking not supported"
  write dev ptr _offset bytes = do
    str <- unsafePackCStringFinalizer ptr bytes (free ptr)
    useAsCString str (unsafe_py_format dev)
  writeNonBlocking dev ptr offset bytes = do
    write dev ptr offset bytes
    return bytes

instance BufferedIO PyDevice where
  newBuffer _ = newByteBuffer 4096
  fillReadBuffer dev _ = error $ show dev ++ ": fillReadBuffer not supported"
  fillReadBuffer0 dev _ = error $ show dev ++ ": fillReadBuffer0 not supported"
  flushWriteBuffer dev buf = do
    let bufStart ptr = castPtr (plusPtr ptr (bufL buf))
    let bufLen = bufR buf - bufL buf
    str <- withForeignPtr (bufRaw buf) $ \ptr -> do
      packCStringLen (bufStart ptr, bufLen)
    useAsCString str (unsafe_py_format dev)
    return (buf {bufL = 0, bufR = 0})
  flushWriteBuffer0 dev buf = do
    newBuf <- flushWriteBuffer dev buf
    return (bufR buf - bufL buf, newBuf)

toStdHandle :: PyDevice -> Handle
toStdHandle PyStdout = stdout
toStdHandle PyStderr = stderr

mkPyHandle :: PyDevice -> IO Handle
mkPyHandle dev = do
  let devName = "<" ++ show dev ++ ">"
  withHandle_ devName (toStdHandle dev) $ \stdHandle__ -> do
    let ioMode = handleTypeToIOMode (haType stdHandle__)
    let textEncoding = haCoded stdHandle__
    let newlineMode = NewlineMode {inputNL = haInputNL stdHandle__, outputNL = haOutputNL stdHandle__}
    mkFileHandleNoFinalizer dev ioMode textEncoding noNewlineTranslation

handleTypeToIOMode :: HandleType -> IOMode
handleTypeToIOMode ReadHandle = ReadMode
handleTypeToIOMode WriteHandle = WriteMode
handleTypeToIOMode ReadWriteHandle = ReadWriteMode
handleTypeToIOMode AppendHandle = AppendMode

py_format_stdout :: String -> IO ()
py_format_stdout str = withCString str unsafe_py_format_stdout

py_format_stderr :: String -> IO ()
py_format_stderr str = withCString str unsafe_py_format_stderr

foreign export ccall hs_example_haskell_wheel_version :: IO CString

hs_example_haskell_wheel_version :: IO CString
hs_example_haskell_wheel_version =
  newCString (showVersion version)

foreign export ccall hs_example_haskell_wheel_main :: IO CInt

exitHandler :: ExitCode -> IO CInt
exitHandler ExitSuccess = return 0
exitHandler (ExitFailure n) = return (fromIntegral n)

uncaughtExceptionHandler :: SomeException -> IO CInt
uncaughtExceptionHandler (SomeException e) =
  py_format_stderr (displayException e)
    >> return 1

hs_example_haskell_wheel_main :: IO CInt
hs_example_haskell_wheel_main =
  handle uncaughtExceptionHandler $
    handle exitHandler $ do
      pyStdout <- mkPyHandle PyStdout
      pyStderr <- mkPyHandle PyStderr
      hDuplicateTo pyStdout stdout
      hDuplicateTo pyStderr stderr
      main
      return 0

foreign export ccall hs_example_haskell_wheel_fib :: CInt -> IO CInt

hs_example_haskell_wheel_fib :: CInt -> IO CInt
hs_example_haskell_wheel_fib =
  return . fromIntegral . fib . fromIntegral

main :: IO ()
main =
  getArgs >>= \args ->
    forM_ args $ \arg -> do
      case readMaybe arg of
        Just n -> py_format_stdout $ "fib " <> show n <> " -> " <> show (fib n)
        Nothing -> py_format_stdout $ "fib " <> arg <> " -> error"

-- Taken from:
-- https://wiki.haskell.org/The_Fibonacci_sequence#Fastest_Fib_in_the_West
fib :: Integer -> Integer
fib n = snd . foldl fib_ (1, 0) . map (toEnum . fromIntegral) $ unfoldl divs n
  where
    unfoldl :: (Integer -> Maybe (Integer, Integer)) -> Integer -> [Integer]
    unfoldl f x = case f x of
      Nothing -> []
      Just (u, v) -> unfoldl f v ++ [u]

    divs :: Integer -> Maybe (Integer, Integer)
    divs 0 = Nothing
    divs k = Just (uncurry (flip (,)) (k `divMod` 2))

    fib_ :: (Integer, Integer) -> Bool -> (Integer, Integer)
    fib_ (f, g) p
      | p = (f * (f + 2 * g), f `pow` 2 + g `pow` 2)
      | otherwise = (f `pow` 2 + g `pow` 2, g * (2 * f - g))

    pow :: Integer -> Integer -> Integer
    pow = (^)
