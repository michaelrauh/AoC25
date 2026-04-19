{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_aoc25 (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
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
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/michaelrauh/.cabal/bin"
libdir     = "/Users/michaelrauh/.cabal/lib/x86_64-osx-ghc-8.6.3/aoc25-0.1.0.0-6Wv5kASS9PI8IfmZg5LCF1-four"
dynlibdir  = "/Users/michaelrauh/.cabal/lib/x86_64-osx-ghc-8.6.3"
datadir    = "/Users/michaelrauh/.cabal/share/x86_64-osx-ghc-8.6.3/aoc25-0.1.0.0"
libexecdir = "/Users/michaelrauh/.cabal/libexec/x86_64-osx-ghc-8.6.3/aoc25-0.1.0.0"
sysconfdir = "/Users/michaelrauh/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "aoc25_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "aoc25_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "aoc25_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "aoc25_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "aoc25_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "aoc25_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
