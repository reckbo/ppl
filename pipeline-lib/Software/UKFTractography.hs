{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Software.UKFTractography
  ( UKFTractographyExe (..)
  , rules
  ) where

import Development.Shake
import Development.Shake.FilePath
import Development.Shake.Command
import Shake.BuildKey
import qualified SoftwareOutputPaths         as Paths
import qualified System.Directory as IO
import Control.Monad (unless)

type GitCommit = String

githubUrl = "https://github.com/pnlbwh/ukftractography.git"

newtype UKFTractographyExe = UKFTractographyExe GitCommit
        deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

instance BuildKey UKFTractographyExe where
  path (UKFTractographyExe hash) = Paths.ukfTractographyExe hash

  build out@(UKFTractographyExe hash) = Just $ do
    tmpdir <- liftIO . IO.makeAbsolute $ takeDirectory (path out)
      </> "UKFTractography-tmp"
    liftIO $ IO.createDirectoryIfMissing True tmpdir
    let srcdir = tmpdir </> "UKFTractography"
        builddir = tmpdir </> "UKFTractography-build"
    repoExists <- liftIO $ IO.doesDirectoryExist srcdir
    unless repoExists $ cmd "git clone" githubUrl srcdir :: Action ()
    cmd [Cwd srcdir] "git checkout" hash :: Action ()
    liftIO $ IO.createDirectoryIfMissing False builddir
    cmd [Cwd builddir] "cmake" srcdir :: Action ()
    cmd [Cwd builddir] "make" :: Action ()
    liftIO $ IO.renameFile (path out) (builddir </> "UKFTractography-build/ukf/bin/UKFTractography")


rules :: Rules ()
rules = do
  rule (buildKey :: UKFTractographyExe -> Maybe (Action [Double]))
