module Paths_ecl (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/niko/.cabal/bin"
libdir     = "/home/niko/.cabal/lib/i386-linux-ghc-7.8.4/ecl-0.1.0.0"
datadir    = "/home/niko/.cabal/share/i386-linux-ghc-7.8.4/ecl-0.1.0.0"
libexecdir = "/home/niko/.cabal/libexec"
sysconfdir = "/home/niko/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "ecl_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "ecl_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "ecl_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "ecl_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "ecl_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
