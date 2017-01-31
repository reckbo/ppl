{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
module Node.UKFTractography
  ( UKFTractographyExe (..)
  , UKFTractography (..)
  , rules
  ) where

import           Control.Monad    (unless, when)
import           FSL              (tobval, tobvec)
import           Node.Dwi         hiding (rules)
import           Node.DwiMask     hiding (rules)
import           Node.Types
import           Node.Util        (getPath, showKey)
import qualified Paths
import           Shake.BuildNode
import qualified System.Directory as IO
import           Util             (convertDwi, convertImage)


newtype UKFTractographyExe = UKFTractographyExe GitHash
        deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

instance BuildNode UKFTractographyExe where
  path n = Paths.softwareDir </> showKey n

  build _ = Nothing

  -- build out@(UKFTractographyExe hash) = Just $ do
  --   clonedir <- liftIO . IO.makeAbsolute $ takeDirectory (path out)
  --     </> "UKFTractography-" ++ hash ++ "-tmp"
  --   buildGitHubCMake [] "pnlbwh/ukftractography" hash clonedir
  --   liftIO $ IO.renameFile (clonedir
  --                           </> "_build"
  --                           </> "UKFTractography-build/ukf/bin/UKFTractography") (path out)
  --   liftIO $ IO.removeDirectoryRecursive clonedir
  --   unit $ cmd "chmod" "a-w" (path out)


newtype UKFTractography
  = UKFTractography (UKFTractographyType, DwiType, DwiMaskType, CaseId)
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
  path n@(UKFTractography (UKFTractographyGiven, _, _, caseid))
    = getPath "ukf" caseid

  path n@(UKFTractography (_, _, _, caseid))
    = Paths.outdir </> caseid </> showKey n <.> "vtk"

  build (UKFTractography (UKFTractographyGiven, _, _, _)) = Nothing

  build n@(UKFTractography (UKFTractographyDefault, dwitype, dwimasktype, caseid))
    = Just $ buildukf defaultParams dwitype dwimasktype caseid (path n)

  build n@(UKFTractography (UKFTractographyCustom params, dwitype, dwimasktype, caseid))
    = Just $ buildukf params dwitype dwimasktype caseid (path n)


buildukf params dwitype dwimasktype caseid out = do
    Just exeNode <- fmap UKFTractographyExe <$> getConfig "UKFTractography-hash"
    let dwi = Dwi (dwitype, caseid)
        dwimask = DwiMask (dwimasktype, dwitype, caseid)
    need exeNode
    need $ dwi
    need $ dwimask
    withTempDir $ \tmpdir -> do
      let dwiNrrd = tmpdir </> "dwi.nrrd"
          dwimaskNrrd = tmpdir </> "dwimask.nrrd"
      Util.convertDwi (path dwi) dwiNrrd
      liftIO $ Util.convertImage (path dwimask) dwimaskNrrd
      command_ [] (path exeNode) (["--dwiFile", dwiNrrd
                                  ,"--maskFile", dwimaskNrrd
                                  ,"--seedsFile", dwimaskNrrd
                                  ,"--recordTensors"
                                  ,"--tracts", out] ++ formatParams params)

rules :: Rules ()
rules = do
  rule (buildNode :: UKFTractographyExe -> Maybe (Action [Double]))
  rule (buildNode :: UKFTractography -> Maybe (Action [Double]))
