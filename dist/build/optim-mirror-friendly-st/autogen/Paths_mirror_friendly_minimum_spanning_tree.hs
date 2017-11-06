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

bindir     = "C:\\Users\\olive\\AppData\\Roaming\\cabal\\bin"
libdir     = "C:\\Users\\olive\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-8.2.1\\mirror-friendly-minimum-spanning-tree-0.1.0.0-8pzoAZyH4Gt2qePLCAL526-optim-mirror-friendly-st"
dynlibdir  = "C:\\Users\\olive\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-8.2.1"
datadir    = "C:\\Users\\olive\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-8.2.1\\mirror-friendly-minimum-spanning-tree-0.1.0.0"
libexecdir = "C:\\Users\\olive\\AppData\\Roaming\\cabal\\mirror-friendly-minimum-spanning-tree-0.1.0.0-8pzoAZyH4Gt2qePLCAL526-optim-mirror-friendly-st\\x86_64-windows-ghc-8.2.1\\mirror-friendly-minimum-spanning-tree-0.1.0.0"
sysconfdir = "C:\\Users\\olive\\AppData\\Roaming\\cabal\\etc"

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
  return (dir ++ "\\" ++ name)
