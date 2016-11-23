module Registration
  (
  ) where

-- import Software.ANTs (ANTs (..))
import System.Process (callProcess)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory, withSystemTempFile)
import System.Directory as IO (copyFile, createDirectoryIfMissing)
import Util (convertImage)
import Data.Foldable (traverse_)
import qualified Teem (isNrrd, gzip, center)
import Control.Monad (when)
import System.Environment (lookupEnv)
import Data.Maybe (fromMaybe)

computeRigid antsPath moving fixed outtxt
  = withSystemTempDirectory "" $ \tmpdir -> do
    let pre = tmpdir </> "ants"
        affine = pre ++ "Affine.txt"
    callProcess (antsPath </> "ANTS") ["3"
                                      ,"-m", "MI["++fixed++","++moving++",1,32]"
                                      ,"-i", "0",
                                       "-o", pre, "--do-rigid"]
    IO.copyFile affine outtxt

applyTransforms antsPath interpolation transforms moving fixed out =
  callProcess (antsPath </> "antsApplyTransforms") $
    ["-d", "3"
    ,"-i", moving
    ,"-o", out
    ,"-r", fixed
    ,"-t"] ++ transforms
    ++ (if null interpolation then [] else ["--interpolation", interpolation])

makeRigidMask antsPath mask moving fixed out
  = withSystemTempFile ".txt" $ \tmpxfm _ -> do
      withSystemTempFile ".nrrd" $ \tmpmask _ -> do
        withSystemTempFile ".nrrd" $ \tmpmoving _ -> do
          traverse_ Teem.center [tmpmask, tmpmoving]
          computeRigid antsPath tmpmoving fixed tmpxfm
          applyTransforms antsPath "NearestNeighbor" [tmpxfm] mask fixed out
          when (Teem.isNrrd out) (Teem.gzip out)

freesurferToDwi fsdir dwi t2 t1 outdir = do
  createDirectoryIfMissing True outdir
  fshome <- fromMaybe (error "freesurferToDWi: Set FREESURFER_HOME") <$> lookupEnv "FREESURFER_HOME"
  callProcess (fshome </> "bin" </> "mri_vol2vol")
    ["--mov", fsdir </> "mri" </> "brain.mgz"
    ,"--targ", fsdir </> "mri" </> "brain.mgz"
    ,"--regheader"
    ,"--o", "brain.nii.gz"]
  callProcess (fshome </> "bin" </> "mri_label2vol")
    ["--seg", "mri" </> "wmparc.mgz"
    ,"--temp", "mri" </> "brain.mgz"
    ,"--regheader", "mri" </> "wmparc.mgz"
    ,"--o", outdir </> "wmparc.nii.gz"]