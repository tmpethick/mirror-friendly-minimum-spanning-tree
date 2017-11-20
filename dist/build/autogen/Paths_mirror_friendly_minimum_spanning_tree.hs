{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_mirror_friendly_minimum_spanning_tree (
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

bindir     = "/home/oliver/.cabal/bin"
libdir     = "/home/oliver/.cabal/lib/x86_64-linux-ghc-8.0.2/mirror-friendly-minimum-spanning-tree-0.1.0.0-ISnA8x0Nzv06YIKn1eNpet"
dynlibdir  = "/home/oliver/.cabal/lib/x86_64-linux-ghc-8.0.2"
datadir    = "/home/oliver/.cabal/share/x86_64-linux-ghc-8.0.2/mirror-friendly-minimum-spanning-tree-0.1.0.0"
libexecdir = "/home/oliver/.cabal/libexec"
sysconfdir = "/home/oliver/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "mirror_friendly_minimum_spanning_tree_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "mirror_friendly_minimum_spanning_tree_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "mirror_friendly_minimum_spanning_tree_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "mirror_friendly_minimum_spanning_tree_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "mirror_friendly_minimum_spanning_tree_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "mirror_friendly_minimum_spanning_tree_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
