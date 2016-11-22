{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module MABS
  ( Mask (..)
  , rules
  ) where

import           Control.Monad            (unless)
import           Data.List                (intercalate)
import           Data.List.Split          (splitOn)
import           Development.Shake.Config
import qualified FSL                      (average, threshold)
import qualified PathsInput               (t1)
import qualified PathsOutput              (t1MaskMabs)
import           PNLUtil                  (convertImage)
import           Shake.BuildNode
import qualified Software.ANTs            as ANTs
import qualified System.Directory         as IO (copyFile)

type CaseId = String

data Mask = Mask CaseId
             deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

instance BuildNode Mask where
  path (Mask caseid) = PathsOutput.t1MaskMabs caseid

  build out@(Mask caseid) = Just $ withTempDir $ \tmpdir -> do
      let t1Target = PathsInput.t1 caseid
      trainingPairs <- map (splitOn ",") <$> readFileLines "config/trainingDataT1.csv"
      need . concat $ trainingPairs
      registeredmasks <- traverse (\[vol,mask] -> register tmpdir t1Target vol mask) trainingPairs
      let tmpnii = tmpdir </>  "tmp.nii.gz"
      FSL.average tmpnii registeredmasks
      FSL.threshold 0.5 tmpnii tmpnii
      liftIO $ PNLUtil.convertImage tmpnii (path out)

rules = rule (buildNode :: Mask -> Maybe (Action [Double]))

register :: FilePath -> FilePath -> FilePath -> FilePath -> Action FilePath
register outdir fixed moving movingMask = withTempDir $ \tmpdir -> do
  let pre = tmpdir </> "moving_to_target"
      warped = pre ++ "Warped.nii.gz"
  ANTs.run "antsRegistrationSyN.sh" ["-d", "3"
                                    ,"-f", fixed
                                    ,"-m", moving
                                    ,"-o", pre
                                    ,"-n", "16"]
  let xfmRigid = pre ++ "0GenericAffine.mat"
      xfmWarp = pre ++ "1Warp.nii.gz"
      xfm = pre ++ "Transform.nii.gz"
  ANTs.run "ComposeMultiTransform" ["3"
                                   ,xfm
                                   ,"-R", fixed
                                   ,xfmWarp
                                   ,xfmRigid]
  let outMask = (intercalate "-" ["Mask", takeBaseName movingMask, "in", takeBaseName moving]) ++ ".nii.gz"
  ANTs.run "antsApplyTransforms" ["-d", "3"
                                 ,"-i", movingMask
                                 ,"-o", outdir </> outMask
                                 ,"-r", fixed
                                 ,"-t", xfm]
  return outMask
