module Paths_hellosnap (
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

bindir     = "/root/.cabal/bin"
libdir     = "/root/.cabal/lib/hellosnap-0.1/ghc-7.4.1"
datadir    = "/root/.cabal/share/hellosnap-0.1"
libexecdir = "/root/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "hellosnap_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "hellosnap_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "hellosnap_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "hellosnap_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
