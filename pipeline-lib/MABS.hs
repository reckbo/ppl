module MABS
  ( mabs
  ) where

import qualified ANTs                     (applyTransforms, computeWarp)
import           Control.Monad            (unless)
import           Data.List                (intercalate)
import           Data.List.Split          (splitOn)
import qualified Development.Shake        as Shake (need)
import qualified FSL                      (average, threshold)
import qualified System.Directory         as IO (copyFile)
import           System.IO.Temp           (withSystemTempFile)
import           Util                     (convertImage)
import Development.Shake
import Development.Shake.FilePath ((</>), (<.>), takeBaseName)
import Teem (isNrrd)
import FSL (isNifti)

mabs :: FilePath -> [[FilePath]] -> FilePath -> FilePath -> Action ()
mabs antsPath trainingPairs t1 out = withTempDir $ \tmpdir -> do
      let ext = if isNrrd(t1) then "nrrd"
                else if isNifti(t1) then "nii.gz"
                     else error $ "mabs: input T1w must be nrrd or nifti: " ++ t1
          tmpt1 = tmpdir </> "t1target" <.> ext
      liftIO $ IO.copyFile t1 tmpt1
      registeredmasks <- traverse
        (\(trainingVol:trainingMask:_) -> register antsPath
          tmpdir (trainingVol, trainingMask) tmpt1)
        trainingPairs
      let tmpnii = tmpdir </> "tmp.nii.gz"
      FSL.average tmpnii registeredmasks
      FSL.threshold 0.5 tmpnii tmpnii
      liftIO $ Util.convertImage tmpnii out

register :: FilePath
         -> FilePath
         -> (FilePath,FilePath)
         -> FilePath
         -> Action FilePath
register antsPath outdir (moving, movingMask) fixed
  = let outMask = outdir </> (intercalate "-" ["Mask"
                                              ,takeBaseName movingMask
                                              ,"in"
                                              ,takeBaseName moving]) <.> "nii.gz"
    in withTempDir $ \tmpdir -> do
      let tmpwarp = tmpdir </> "warp.nii.gz"
      ANTs.computeWarp antsPath moving fixed tmpwarp
      ANTs.applyTransforms antsPath
        "NearestNeighbor" [tmpwarp] movingMask fixed outMask
      return outMask
