module Paths_AbstractInterpretation (
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

bindir     = "/Users/dearkx/.cabal/bin"
libdir     = "/Users/dearkx/.cabal/lib/x86_64-osx-ghc-7.10.2/Abstr_F0BLkTw88a5H4fQHomexvg"
datadir    = "/Users/dearkx/.cabal/share/x86_64-osx-ghc-7.10.2/AbstractInterpretation-0.1.0.0"
libexecdir = "/Users/dearkx/.cabal/libexec"
sysconfdir = "/Users/dearkx/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "AbstractInterpretation_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "AbstractInterpretation_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "AbstractInterpretation_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "AbstractInterpretation_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "AbstractInterpretation_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
