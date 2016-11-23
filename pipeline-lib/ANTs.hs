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

computeRigid antsPath moving fixed outtxt
  = withSystemTempDirectory "" $ \tmpdir -> do
    let pre = tmpdir </> "ants"
        affine = pre ++ "Affine.txt"
    callProcess (antsPath </> "ANTS") ["3"
                                      ,"-m", "MI["++fixed++","++moving++",1,32]"
                                      ,"-i", "0",
                                       "-o", pre, "--do-rigid"]
    IO.copyFile affine outtxt

-- TODO replace antsIntroduction.sh with direct call to ANTs
computeWarp antsPath metric moving fixed outwarp
  = withSystemTempDirectory "" $ \tmpdir -> do
    let pre = tmpdir </> "ants"
        affine = pre ++ "Affine.txt"
        warp = pre ++ "Warp.nii.gz"
    callProcess (antsPath </> "antsIntroduction.sh")
      ["-d", "3"
      ,"-i", moving
      ,"-o", pre
      ,"-s", metric]
    callProcess (antsPath </> "ComposeMultiTransform")
      ["3", outwarp
      ,"-R", warp, affine]

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
