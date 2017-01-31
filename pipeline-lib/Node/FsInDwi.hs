{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}
module Node.FsInDwi
  (FsInDwi (..)
  ,rules
  ) where

import qualified ANTs
import           Data.Foldable       (traverse_)
import           Data.Maybe          (fromMaybe)
import           Node.ANTs           hiding (rules)
import           Node.Dwi            hiding (rules)
import           Node.DwiMask        hiding (rules)
import           Node.FreeSurfer     hiding (rules)
import           Node.Structural     hiding (rules)
import           Node.StructuralMask hiding (rules)
import Node.Software.BrainsTools hiding (rules)
import           Node.Types
import           Node.Util           (showKey)
import qualified Paths
import           Shake.BuildNode
import           System.Directory    as IO (renameFile)
import           System.Environment  (lookupEnv)
import qualified Util                (extractB0, maskImage)

data FsInDwi =
  FsInDwi {bthash      :: GitHash
          ,fs2dwitype  :: FsToDwiType
          ,fstype      :: FreeSurferType
          ,dwitype     :: DwiType
          ,dwimasktype :: DwiMaskType
          ,caseid      :: CaseId}
  deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

instance BuildNode FsInDwi where
  path n@(FsInDwi{..}) = Paths.outdir </> caseid </> showKey n <.> "nii.gz"
  build out@(FsInDwi bthash (FsBrain_T1_T2_B0 strcttype strctmasktype) fstype dwitype dwimasktype caseid) =
    Just $
    withTempDir $
    \tmpdir ->
      do let t2 = Structural T2w caseid
             t1 = Structural T1w caseid
             fsInDwiDir = (dropExtensions $ path out)
         need FreeSurfer {..}
         need Dwi {..}
         need DwiMask {..}
         need StructuralMask {..}
         need t2
         need t1
         let t2mask = tmpdir </> "t2mask.nii.gz"
         (t1mask,t2mask) <-
           case strcttype of
             T2w ->
               do liftIO $
                    ANTs.makeRigidMask (pathDir BrainsTools {..})
                                       (path StructuralMask {..})
                                       (path t2)
                                       (path t1)
                                       (tmpdir </> "t1mask.nii.gz")
                  return (tmpdir </> "t1mask.nii.gz",path StructuralMask {..})
             T1w ->
               do liftIO $
                    ANTs.makeRigidMask (pathDir BrainsTools {..})
                                       (path StructuralMask {..})
                                       (path t1)
                                       (path t2)
                                       (tmpdir </> "t2mask.nii.gz")
                  return (path StructuralMask {..},tmpdir </> "t2mask.nii.gz")
         ANTs.freesurferToDwiWithMasks (pathDir BrainsTools {..})
                                       (pathDir FreeSurfer {..})
                                       (path Dwi {..})
                                       (path DwiMask {..})
                                       (path t1)
                                       t1mask
                                       (path t2)
                                       t2mask
                                       fsInDwiDir
         liftIO $
           IO.renameFile (fsInDwiDir </> "wmparc-in-dwi.nii.gz")
                         (path out)
  build n@(FsInDwi bthash FsBrain_B0 fstype dwitype dwimasktype caseid) =
    Just $
    withTempDir $
    \tmpdir ->
      do fshome <-
           liftIO $
           fromMaybe (error "freesurferToDwi: Set FREESURFER_HOME") <$>
           lookupEnv "FREESURFER_HOME"
         let b0 = tmpdir </> "b0.nii.gz"
             maskedb0 = tmpdir </> "maskedb0.nii.gz"
             brain = tmpdir </> "brain.nii.gz"
             wmparc = tmpdir </> "wmparc.nii.gz"
             brainmgz = head . paths $ FreeSurfer {..}
             wmparcmgz = last . paths $ FreeSurfer {..}
         need FreeSurfer {..}
         need Dwi {..}
         need DwiMask {..}
         unit $
           cmd (AddEnv "SUBJECTS_DIR" "")
               (fshome </> "bin" </> "mri_vol2vol")
               ["--mov",brainmgz,"--targ",brainmgz,"--regheader","--o",brain]
         unit $
           cmd (AddEnv "SUBJECTS_DIR" "")
               (fshome </> "bin" </> "mri_label2vol")
               ["--seg"
               ,wmparcmgz
               ,"--temp"
               ,brainmgz
               ,"--regheader"
               ,wmparcmgz
               ,"--o"
               ,wmparc]
         Util.extractB0 (path Dwi {..})
                        b0
         liftIO $
           Util.maskImage b0
                          (path DwiMask {..})
                          maskedb0
         let pre = tmpdir </> "fsbrain_to_b0"
             affine = pre ++ "0GenericAffine.mat"
             warp = pre ++ "1Warp.nii.gz"
         unit $
           cmd (pathDir BrainsTools {..} </> "antsRegistration")
               (ANTs.defaultParams ++
                ["--output","[" ++ pre ++ "]"] ++
                ANTs.warpStages ANTs.MI brain maskedb0)
         liftIO $
           ANTs.applyTransforms (pathDir BrainsTools {..})
                                "NearestNeighbor"
                                [warp,affine]
                                (last . paths $ FreeSurfer {..})
                                maskedb0
                                (path n)


rules = rule (buildNode :: FsInDwi -> Maybe (Action [Double]))
