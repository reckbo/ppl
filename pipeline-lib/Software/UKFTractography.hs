{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Software.UKFTractography
  ( UKFTractographyExe (..)
  , rules
  ) where

import Development.Shake
import Development.Shake.FilePath
import Development.Shake.Command
import Shake.BuildNode
import qualified SoftwareOutputPaths         as Paths
import qualified System.Directory as IO
import Control.Monad (unless, when)

type GitCommit = String

url = "https://github.com/pnlbwh/ukftractography.git"

newtype UKFTractographyExe = UKFTractographyExe GitCommit
        deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

instance BuildNode UKFTractographyExe where
  path (UKFTractographyExe hash) = Paths.ukfTractographyPrefix ++ "-" ++  hash

  build out@(UKFTractographyExe hash) = Just $ do
    tmpdir <- liftIO . IO.makeAbsolute $ takeDirectory (path out)
      </> "UKFTractography-tmp"
    liftIO $ IO.createDirectoryIfMissing True tmpdir
    let clonedir = tmpdir </> "UKFTractography"
        builddir = tmpdir </> "UKFTractography-build"
    cloneExists <- liftIO $ IO.doesDirectoryExist clonedir
    cmakeListsExists <- liftIO $ IO.doesFileExist (clonedir </> "CMakeLists.txt")
    unless cmakeListsExists
      (do
          liftIO $ when cloneExists $ IO.removeDirectoryRecursive clonedir
          cmd "git clone" url clonedir :: Action ()
      )
    cmd [Cwd clonedir] "git checkout" hash :: Action ()
    liftIO $ IO.createDirectoryIfMissing False builddir
    cmd [Cwd builddir] "cmake" clonedir :: Action ()
    cmd [Cwd builddir] "make -j6" :: Action ()
    liftIO $ IO.renameFile (builddir </> "UKFTractography-build/ukf/bin/UKFTractography") (path out)
    liftIO $ IO.removeDirectoryRecursive tmpdir


rules :: Rules ()
rules = rule (buildNode :: UKFTractographyExe -> Maybe (Action [Double]))
