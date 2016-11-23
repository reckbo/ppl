{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module BuildNode.UKFTractography
  ( UKFTractographyExe (..)
  , rules
  , run
  ) where

import           Control.Monad              (unless, when)
import           Development.Shake
import           Development.Shake.Command
import           Development.Shake.FilePath
import           Development.Shake.Config
import qualified PathsOutput                as Paths
import           Shake.BuildNode
import           Util              (buildGitHubCMake)
import qualified System.Directory           as IO

newtype UKFTractographyExe = UKFTractographyExe GitHash
        deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

instance BuildNode UKFTractographyExe where
  path (UKFTractographyExe hash) = Paths.ukfTractographyPrefix ++ "-" ++  hash

  build out@(UKFTractographyExe hash) = Just $ do
    clonedir <- liftIO . IO.makeAbsolute $ takeDirectory (path out)
      </> "UKFTractography-" ++ hash ++ "-tmp"
    liftIO $ buildGitHubCMake [] "pnlbwh/ukftractography" hash clonedir
    liftIO $ IO.renameFile (clonedir
                            </> "_build"
                            </> "UKFTractography-build/ukf/bin/UKFTractography") (path out)
    liftIO $ IO.removeDirectoryRecursive clonedir

rules :: Rules ()
rules = rule (buildNode :: UKFTractographyExe -> Maybe (Action [Double]))

defaultParams=["--numTensor", "2"
              ,"--seedsPerVoxel", "10"
              ,"--Qm", "0.001"
              ,"--Ql", "70"
              ,"--Rs", "0.015"
              ,"--stepLength", "0.3"
              ,"--seedFALimit", "0.18"
              ,"--recordLength", "1.7"]

run :: FilePath -> FilePath -> FilePath -> Action ()
run dwi mask out = do
  Just hash <- getConfig "UKFTractography-hash"
  apply1 (UKFTractographyExe hash) :: Action [Double]
  cmd (path $ UKFTractographyExe hash)
    (["--dwiFile", dwi
     ,"--maskFile", mask
     ,"--seedsFile", mask
     ,"--recordTensors"
     ,"--tracts", out]
    ++ defaultParams)
