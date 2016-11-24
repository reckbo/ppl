module ANTs
  (computeRigid
  ,computeWarp
  ,applyTransforms
  ,upsample
  ) where

-- import Software.ANTs (ANTs (..))
import           Data.Maybe       (fromMaybe)
import           System.Directory as IO (copyFile)
import           System.FilePath  ((</>))
import           System.IO.Temp   (withSystemTempDirectory, withSystemTempFile)
import           System.Process   (callProcess)

-- TODO replace ANTS with newer antsRegistration
computeRigid antsPath moving fixed outtxt
  = withSystemTempDirectory "" $ \tmpdir -> do
    let pre = tmpdir </> "ants"
        affine = pre ++ "Affine.txt"
    callProcess (antsPath </> "ANTS") ["3"
                                      ,"-m", "MI["++fixed++","++moving++",1,32]"
                                      ,"-i", "0",
                                       "-o", pre, "--do-rigid"]
    IO.copyFile affine outtxt

computeWarp antsPath moving fixed outwarp
  = withSystemTempDirectory "" $ \tmpdir -> do
    let pre = tmpdir </> "ants"
        affine = pre ++ "0GenericAffine.mat"
        warp = pre ++ "1Warp.nii.gz"
    -- antsRegistrationSyN uses MI for the Rigid and Affine stages,
    -- and CC with radius 4 for the non-linear BSplineSyN stage
    callProcess (antsPath </> "antsRegistrationSyN.sh")
      ["-d", "3"
      ,"-f", fixed
      ,"-m", moving
      ,"-o", pre
      ,"-n", "16"]
    callProcess (antsPath </> "ComposeMultiTransform")
      ["3", outwarp ,"-R", fixed, warp, affine]

applyTransforms antsPath interpolation transforms moving fixed out =
  callProcess (antsPath </> "antsApplyTransforms") $
    ["-d", "3"
    ,"-i", moving
    ,"-o", out
    ,"-r", fixed
    ,"-t"] ++ transforms
    ++ (if null interpolation then [] else ["--interpolation", interpolation])

upsample antsPath spacings img out
  = callProcess (antsPath </> "ResampleImageBySpacing")
  ["3", img, out, unwords . map show $ spacings]
