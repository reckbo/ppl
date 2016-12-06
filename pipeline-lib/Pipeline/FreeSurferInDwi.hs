{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
module Pipeline.FreeSurferInDwi
  ( rules
  , FsInDwi (..)
  , FsToDwiType (..)
  ) where

import           System.Environment  (lookupEnv)
import           Data.Maybe          (fromMaybe)
import           qualified ANTs
import           Data.Foldable           (traverse_)
import qualified Paths
import           Pipeline.ANTs           hiding (rules)
import           Pipeline.DWI            hiding (rules)
import           Pipeline.DWIMask        hiding (rules)
import           Pipeline.FreeSurfer     hiding (rules)
import           Pipeline.Structural     hiding (rules)
import           Pipeline.StructuralMask hiding (rules)
import           Pipeline.Util           (showKey)
import           Shake.BuildNode
import           System.Directory        as IO (renameFile)
import qualified Util                    (extractB0, maskImage)

type CaseId = String

data FsToDwiType = FsBrain_T1_T2_B0 StructuralType StructuralMaskType
                 | FsBrain_B0
                 deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

newtype FsInDwi = FsInDwi (FsToDwiType, FreeSurferType, DwiType, DwiMaskType, CaseId)
             deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

instance BuildNode FsInDwi where
  path n@(FsInDwi (_, _, _, _, caseid)) = Paths.fsInDwiDir caseid </> showKey n <.> "nii.gz"

  build node@(FsInDwi (FsBrain_T1_T2_B0 strcttype strctmasktype, fstype, dwitype
                      , dwimaskType, caseid)) = Just $ withTempDir $ \tmpdir -> do
      antspath <- Pipeline.ANTs.getAntsPath
      let fsN = FreeSurfer (fstype, caseid)
          dwiN = Dwi (dwitype, caseid)
          dwiMaskN = DwiMask (dwimaskType, dwitype, caseid)
          t2N = Structural (T2w, caseid)
          t1N = Structural (T1w, caseid)
          maskN = StructuralMask (strctmasktype, strcttype, caseid)
          fsInDwiDir = (dropExtensions $ path node)
      need fsN
      need dwiN
      need dwiMaskN
      need t2N
      need t1N
      need maskN
      let t2mask = tmpdir </> "t2mask.nii.gz"
      (t1mask, t2mask) <- case strcttype of
        T2w -> do
          liftIO $ ANTs.makeRigidMask antspath (path maskN) (path t2N) (path t1N) (tmpdir </> "t1mask.nii.gz")
          return (tmpdir </> "t1mask.nii.gz", path maskN)
        T1w -> do
          liftIO $ ANTs.makeRigidMask antspath (path maskN) (path t1N) (path t2N) (tmpdir </> "t2mask.nii.gz")
          return (path maskN, tmpdir </> "t2mask.nii.gz")
      ANTs.freesurferToDwiWithMasks antspath
        (pathDir fsN)
        (path dwiN) (path dwiMaskN)
        (path t1N) t1mask
        (path t2N) t2mask
        fsInDwiDir
      liftIO $ IO.renameFile (fsInDwiDir </> "wmparc-in-dwi.nii.gz") (path node)

  build n@(FsInDwi (FsBrain_B0, fstype, dwitype , dwimaskType, caseid))
    = Just $ withTempDir $ \tmpdir -> do
      antspath <- Pipeline.ANTs.getAntsPath
      fshome <- liftIO $ fromMaybe (error "freesurferToDwi: Set FREESURFER_HOME") <$> lookupEnv "FREESURFER_HOME"
      let fsN = FreeSurfer (fstype, caseid)
          dwiN = Dwi (dwitype, caseid)
          dwiMaskN = DwiMask (dwimaskType, dwitype, caseid)
          b0 = tmpdir </> "b0.nii.gz"
          maskedb0 = tmpdir </> "maskedb0.nii.gz"
          brain = tmpdir </> "brain.nii.gz"
          wmparc = tmpdir </> "wmparc.nii.gz"
          brainmgz = head . paths $ fsN
          wmparcmgz = last . paths $ fsN
      need fsN
      need dwiN
      need dwiMaskN
      unit $ cmd (AddEnv "SUBJECTS_DIR" "") (fshome </> "bin" </> "mri_vol2vol")
        ["--mov", brainmgz
        ,"--targ", brainmgz
        ,"--regheader"
        ,"--o", brain]
      unit $ cmd (AddEnv "SUBJECTS_DIR" "")  (fshome </> "bin" </> "mri_label2vol")
        ["--seg", wmparcmgz
        ,"--temp", brainmgz
        ,"--regheader", wmparcmgz
        ,"--o", wmparc]
      Util.extractB0 (path dwiN) b0
      liftIO $ Util.maskImage b0 (path dwiMaskN) maskedb0
      let pre = tmpdir </> "fsbrain_to_b0"
          affine = pre ++ "0GenericAffine.mat"
          warp = pre ++ "1Warp.nii.gz"
      unit $ cmd (antspath </> "antsRegistration")
        (ANTs.defaultParams
         ++ ["--output", "["++pre++"]"]
         ++ ANTs.warpStages ANTs.MI brain maskedb0
        )
      liftIO $ ANTs.applyTransforms antspath "NearestNeighbor"
        [warp, affine] (last . paths $ fsN) maskedb0 (path n)


rules = rule (buildNode :: FsInDwi -> Maybe (Action [Double]))
