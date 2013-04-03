module Paths_mlax (
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
libdir     = "/home/los/workspace/MiniLAX/.dist-buildwrapper/cabal-dev//lib/mlax-0.1/ghc-7.4.2"
datadir    = "/home/los/workspace/MiniLAX/.dist-buildwrapper/cabal-dev//share/mlax-0.1"
libexecdir = "/home/los/workspace/MiniLAX/.dist-buildwrapper/cabal-dev//libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "mlax_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "mlax_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "mlax_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "mlax_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
