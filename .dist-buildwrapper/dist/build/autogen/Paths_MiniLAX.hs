module Paths_MiniLAX (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,1], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/los/workspace/MiniLAX/.dist-buildwrapper/cabal-dev//bin"
libdir     = "/home/los/workspace/MiniLAX/.dist-buildwrapper/cabal-dev//lib/MiniLAX-0.1/ghc-7.4.2"
datadir    = "/home/los/workspace/MiniLAX/.dist-buildwrapper/cabal-dev//share/MiniLAX-0.1"
libexecdir = "/home/los/workspace/MiniLAX/.dist-buildwrapper/cabal-dev//libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "MiniLAX_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "MiniLAX_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "MiniLAX_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "MiniLAX_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
