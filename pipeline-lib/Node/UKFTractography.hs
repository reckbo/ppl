{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
module Node.UKFTractography
  ( UKFTractographyExe (..)
  , UKFTractographyType (..)
  , UKFTractography (..)
  , rules
  ) where

import           Control.Monad    (unless, when)
import           Node.DWI         hiding (rules)
import           Node.DWIMask     hiding (rules)
import           Node.Util        (showKey, getPath)
import qualified Paths
import           Shake.BuildNode
import qualified System.Directory as IO
import           Util             (buildGitHubCMake)


newtype UKFTractographyExe = UKFTractographyExe GitHash
        deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

instance BuildNode UKFTractographyExe where
  path (UKFTractographyExe hash) = Paths.outdir </> "UKFTractography-" ++  hash

  build out@(UKFTractographyExe hash) = Just $ do
    clonedir <- liftIO . IO.makeAbsolute $ takeDirectory (path out)
      </> "UKFTractography-" ++ hash ++ "-tmp"
    buildGitHubCMake [] "pnlbwh/ukftractography" hash clonedir
    liftIO $ IO.renameFile (clonedir
                            </> "_build"
                            </> "UKFTractography-build/ukf/bin/UKFTractography") (path out)
    liftIO $ IO.removeDirectoryRecursive clonedir
    unit $ cmd "chmod" "a-w" (path out)

type CaseId = String

type Params = [(String, String)]

data UKFTractographyType = UKFTractographyDefault DwiType DwiMaskType
                         | UKFTractographyCustom Params DwiType DwiMaskType
                         | UKFTractographyGiven
        deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

newtype UKFTractography
  = UKFTractography (UKFTractographyType, CaseId)
  deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

defaultParams :: Params
defaultParams = [("Ql","70")
               ,("Qm","0.001")
               ,("Rs","0.015")
               ,("numTensor","2")
               ,("recordLength","1.7")
               ,("seedFALimit","0.18")
               ,("seedsPerVoxel","10")
               ,("stepLength","0.3")]

formatParams :: Params -> [String]
formatParams ps = concatMap (\(arg,val) -> ["--"++arg,val]) ps

bsub_ opts exe args = command_ opts "bsub" $ ["-K"
                                            ,"-n","8"
                                            ,"-R","rusage[mem=8000]"
                                            ,"-o","%J.out"
                                            ,"-e","%J.err"
                                            ,"-q","big-multi"] ++ [exe] ++ args

instance BuildNode UKFTractography  where
  path n@(UKFTractography (UKFTractographyGiven, caseid))
    = getPath "ukf" caseid

  path n@(UKFTractography (_, caseid))
    = Paths.outdir </> caseid </> showKey n <.> "vtk"

  build (UKFTractography (UKFTractographyGiven, _)) = Nothing

  build n@(UKFTractography (UKFTractographyDefault dwitype dwimasktype, caseid))
    = Just $ buildukf defaultParams dwitype dwimasktype caseid (path n)

  build n@(UKFTractography (UKFTractographyCustom params dwitype dwimasktype, caseid))
    = Just $ buildukf params dwitype dwimasktype caseid (path n)


buildukf params dwitype dwimasktype caseid out = do
    Just exeNode <- fmap UKFTractographyExe <$> getConfig "UKFTractography-hash"
    need exeNode
    need $ Dwi (dwitype, caseid)
    need $ DwiMask (dwimasktype, caseid)
    command_ [] (path exeNode) (["--dwiFile", path $ Dwi (dwitype, caseid)
                        ,"--maskFile", path $ DwiMask (dwimasktype, caseid)
                        ,"--seedsFile", path $ DwiMask (dwimasktype, caseid)
                        ,"--recordTensors"
                        ,"--tracts", out] ++ formatParams params)

rules :: Rules ()
rules = do
  rule (buildNode :: UKFTractographyExe -> Maybe (Action [Double]))
  rule (buildNode :: UKFTractography -> Maybe (Action [Double]))
