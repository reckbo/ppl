{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Software.UKFTractography
  ( UKFTractographyExe (..)
  , rules
  ) where

import           Control.Monad              (unless, when)
import           Development.Shake
import           Development.Shake.Command
import           Development.Shake.FilePath
import qualified OutputPaths                as Paths
import           Shake.BuildNode
import           Software.Util              (buildGitHubCMake)
import qualified System.Directory           as IO

newtype UKFTractographyExe = UKFTractographyExe GitHash
        deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

instance BuildNode UKFTractographyExe where
  path (UKFTractographyExe hash) = Paths.ukfTractographyPrefix ++ "-" ++  hash

  build out@(UKFTractographyExe hash) = Just $ do
    clonedir <- liftIO . IO.makeAbsolute $ takeDirectory (path out)
      </> "UKFTractography-" ++ hash ++ "-tmp"
    buildGitHubCMake [] "pnlbwh/ukftractography" hash clonedir
    liftIO $ IO.renameFile (clonedir
                            </> "_build"
                            </> "UKFTractography-build/ukf/bin/UKFTractography") (path out)
    liftIO $ IO.removeDirectoryRecursive clonedir


rules :: Rules ()
rules = rule (buildNode :: UKFTractographyExe -> Maybe (Action [Double]))
