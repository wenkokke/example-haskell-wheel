{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_fib (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude


#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/Users/keri/Projects/haskell-wheel/build/temp.macosx-13-arm64-cpython-311/bin"
libdir     = "aarch64-osx-ghc-9.4.4/fib-0.1.0-AVSGmAnxQKzBTijTXwKLIr-_binding"
dynlibdir  = ""
datadir    = "aarch64-osx-ghc-9.4.4/fib-0.1.0"
libexecdir = "/Users/keri/Projects/haskell-wheel/build/temp.macosx-13-arm64-cpython-311/libexec/aarch64-osx-ghc-9.4.4/fib-0.1.0"
sysconfdir = "/Users/keri/Projects/haskell-wheel/build/temp.macosx-13-arm64-cpython-311/etc"

getBinDir     = catchIO (getEnv "fib_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "fib_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "fib_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "fib_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "fib_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "fib_sysconfdir") (\_ -> return sysconfdir)




joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
