{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_Hasculator (
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
version = Version [0,2,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/nero/.cabal/bin"
libdir     = "/home/nero/.cabal/lib/x86_64-linux-ghc-8.0.2/Hasculator-0.2.0-inplace-HUnitTestSuite"
dynlibdir  = "/home/nero/.cabal/lib/x86_64-linux-ghc-8.0.2"
datadir    = "/home/nero/.cabal/share/x86_64-linux-ghc-8.0.2/Hasculator-0.2.0"
libexecdir = "/home/nero/.cabal/libexec/x86_64-linux-ghc-8.0.2/Hasculator-0.2.0"
sysconfdir = "/home/nero/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Hasculator_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Hasculator_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "Hasculator_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "Hasculator_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Hasculator_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Hasculator_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
