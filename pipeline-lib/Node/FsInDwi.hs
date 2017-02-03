{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}
module Node.FsInDwi
  (FsInDwi (..)
  ,rules
  ) where

import qualified ANTs
import           Data.Foldable             (traverse_)
import           Data.Maybe                (fromMaybe)
import           Node.Dwi                  hiding (rules)
import           Node.DwiMask              hiding (rules)
import           Node.FreeSurfer           hiding (rules)
import           Node.Software.BrainsTools hiding (rules)
import           Node.T1w                  hiding (rules)
import           Node.T1wMask              hiding (rules)
import           Node.T2w                  hiding (rules)
import           Node.Types
import           Node.Util                 (showKey)
import qualified Paths
import           Shake.BuildNode
import           System.Directory          as IO (renameFile)
import           System.Environment        (lookupEnv)
import qualified Util                      (extractB0, maskImage)

data FsInDwi =
  FsInDwi {bthash        :: GitHash
          ,fs2dwimethod  :: FsToDwiMethod
          ,fstype        :: FreeSurferType
          ,dwitype       :: DwiType
          ,dwimaskmethod :: DwiMaskMethod
          ,caseid        :: CaseId}
  deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

instance BuildNode FsInDwi where
  path n@(FsInDwi{..}) = Paths.outdir </> caseid </> showKey n <.> "nii.gz"
  -- build out@(FsInDwi bthash (FsBrain_T1_T2_B0 strcttype strctmasktype) fstype dwitype dwimasktype caseid) =
  build out@(FsInDwi{..}) =
    case fs2dwimethod of
      (FsBrain_T1_T2_B0 t1type t2type t1masktype) ->
        Just $
        withTempDir $
        \tmpdir ->
          do let fsInDwiDir = (dropExtensions $ path out)
                 t2mask = tmpdir </> "t2mask.nrrd"
             need FreeSurfer {..}
             need Dwi {..}
             need DwiMask {..}
             need T1wMask {..}
             need T1w {..}
             need T2w {..}
             ANTs.makeRigidMask (pathDir BrainsTools{..}) (path T1wMask{..}) (path T1w{..}) (path T2w{..}) t2mask
             ANTs.freesurferToDwiWithMasks (pathDir BrainsTools {..})
                       (pathDir FreeSurfer{..})
                       (path Dwi{..})
                       (path DwiMask{..})
                       (path T1w{..})
                       (path T1wMask{..})
                       t2mask
                       (path T2w{..})
                       fsInDwiDir
             liftIO $ IO.renameFile (fsInDwiDir </> "wmparc-in-dwi.nii.gz")
                           (path out)
      FsBrain_B0 ->
        Just . withTempDir $
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
                   ["--mov"
                   ,brainmgz
                   ,"--targ"
                   ,brainmgz
                   ,"--regheader"
                   ,"--o"
                   ,brain]
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
             ANTs.applyTransforms (pathDir BrainsTools {..})
                                  "NearestNeighbor"
                                  [warp,affine]
                                  (last . paths $ FreeSurfer {..})
                                  maskedb0
                                  (path out)


rules = rule (buildNode :: FsInDwi -> Maybe (Action [Double]))
