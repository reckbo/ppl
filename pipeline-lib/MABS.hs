{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module MABS
  (Mask (..)
  ,rules
  ) where


import Control.Monad (unless)
import           Data.List.Split        (splitOn)
import Data.List (intercalate)
import qualified FSL              (average, threshold)
import qualified InputPaths       (t1)
import qualified OutputPaths      (t1MaskMabs)
import           Shake.BuildNode
import qualified System.Directory as IO (copyFile)
import Development.Shake.Config
import qualified Software.ANTs as ANTs

type CaseId = String

data Mask = Mask CaseId
             deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

instance BuildNode Mask where
  path (Mask caseid) = OutputPaths.t1MaskMabs caseid

  build out@(Mask caseid) = Just $ withTempDir $ \tmpdir -> do
      let t1Target = InputPaths.t1 caseid
      trainingPairs <- map (splitOn ",") <$> readFileLines "config/trainingDataT1.csv"
      need . concat $ trainingPairs
      registeredmasks <- traverse (\[vol,mask] -> register t1Target vol mask) trainingPairs
      let tmpnii = tmpdir </>  "tmp.nii.gz"
      FSL.average tmpnii registeredmasks
      FSL.threshold 0.5 tmpnii tmpnii
      unless (takeExtensions (path out) == ".nii.gz") (
        cmd "ConvertBetweenFileFormats" tmpnii (path out)
        )

rules = rule (buildNode :: Mask -> Maybe (Action [Double]))

register :: FilePath -> FilePath -> FilePath -> Action FilePath
register fixed moving movingMask = withTempDir $ \tmpdir -> do
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
  let outMask = (intercalate "-" [takeBaseName movingMask, "in", takeBaseName moving]) ++ ".nii.gz"
  ANTs.run "antsApplyTransforms" ["-d", "3"
                                 ,"-i", movingMask
                                 ,"-o", outMask
                                 ,"-r", fixed
                                 ,"-t", xfm]
  return outMask
