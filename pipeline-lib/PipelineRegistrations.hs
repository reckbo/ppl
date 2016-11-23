module PipelineRegistrations
  (makeRigidMask
  ,freesurferToDwi
  ) where

import           ANTs
import           Control.Monad      (when)
import           Data.Foldable      (traverse_)
import           Data.Maybe         (fromMaybe)
import           System.Directory   as IO (copyFile, createDirectoryIfMissing)
import           System.Environment (lookupEnv)
import           System.FilePath    ((</>))
import           System.IO.Temp     (withSystemTempDirectory,
                                     withSystemTempFile)
import           System.Process     (callProcess)
import qualified Teem               (center, extractB0, gzip, isNrrd)
import           Util               (convertImage)

makeRigidMask antsPath mask moving fixed out
  = withSystemTempFile ".txt" $ \tmpxfm _ -> do
      withSystemTempFile ".nrrd" $ \tmpmask _ -> do
        withSystemTempFile ".nrrd" $ \tmpmoving _ -> do
          traverse_ Teem.center [tmpmask, tmpmoving]
          computeRigid antsPath tmpmoving fixed tmpxfm
          applyTransforms antsPath "NearestNeighbor" [tmpxfm] mask fixed out
          when (Teem.isNrrd out) (Teem.gzip out)

freesurferToDwi antsPath fsdir dwi t2 t1 outdir = do
  createDirectoryIfMissing True outdir
  fshome <- fromMaybe (error "freesurferToDwi: Set FREESURFER_HOME") <$> lookupEnv "FREESURFER_HOME"
  let brain = outdir </> "brain.nii.gz"
      wmparc = outdir </> "wmparc.nii.gz"
      bse = outdir </> "bse.nrrd"
      fsToT1_rigid = outdir </> "fsToT1-rigid.txt"
      t1ToT2_rigid = outdir </> "t1ToT2-rigid.txt"
      t2ToDwi_warp = outdir </> "t2ToDwi-warp.nii.gz"
      wmparcInDwi = outdir </> "wmparc-in-dwi.nrrd"
  callProcess (fshome </> "bin" </> "mri_vol2vol")
    ["--mov", fsdir </> "mri" </> "brain.mgz"
    ,"--targ", fsdir </> "mri" </> "brain.mgz"
    ,"--regheader"
    ,"--o", brain]
  callProcess (fshome </> "bin" </> "mri_label2vol")
    ["--seg", "mri" </> "wmparc.mgz"
    ,"--temp", "mri" </> "brain.mgz"
    ,"--regheader", "mri" </> "wmparc.mgz"
    ,"--o", wmparc]
  -- Make upsampled DWI b0
  withSystemTempFile ".nrrd" $ \dwinrrd _ -> do
    convertImage dwi dwinrrd
    Teem.extractB0 dwinrrd bse
  upsample antsPath [1,1,1] bse bse
  -- Compute transforms from brain.nii.gz to T1
  computeRigid antsPath brain t1 fsToT1_rigid
  computeRigid antsPath t1 t2 t1ToT2_rigid
  computeWarp antsPath "CC" t2 bse t2ToDwi_warp
  -- Apply transforms to wmparc
  applyTransforms antsPath "NearestNeighbor"
    [t2ToDwi_warp, t1ToT2_rigid, fsToT1_rigid]
    wmparc dwi wmparcInDwi
  callProcess "ConvertBetweenFileFormats" [wmparcInDwi, wmparcInDwi, "short"]