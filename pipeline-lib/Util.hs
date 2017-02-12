module Util
  (convertImage
  -- ,maskImage
  -- ,extractB0
  ) where

import           Shake.BuildNode
import qualified System.Directory as IO (copyFile)
import           System.FilePath  (takeExtensions)
-- import           System.IO.Temp   (withSystemTempDirectory)
-- import           System.Process   (CreateProcess (..), callProcess,
                                   -- createProcess, proc)

convertImage :: FilePath -> FilePath -> Action ()
convertImage infile outfile
  = if takeExtensions infile == takeExtensions outfile then
      liftIO $ IO.copyFile infile outfile
    else
      command_ [] "ConvertBetweenFileFormats" [infile, outfile]

-- maskImage :: FilePath -> FilePath -> FilePath -> IO ()
-- maskImage img mask out | FSL.isNifti out = maskImageUsing "nii.gz" FSL.mask out
--                        | Teem.isNrrd out = maskImageUsing "nrrd" Teem.mask out
--                        | otherwise = error "maskImage: images must be in Nrrd or Nifti format"
--                        where
--                          maskImageUsing ext maskFn out = withSystemTempDirectory "maskImage" $
--                            \tmpdir -> do
--                              let tmpmask = tmpdir </> "mask" <.> ext
--                                  tmpimg = tmpdir </> "tmpimg" <.> ext
--                              convertImage mask tmpmask
--                              convertImage img tmpimg
--                              maskFn tmpimg tmpmask out

-- extractB0 :: FilePath -> FilePath -> Action ()
-- extractB0 dwi out | FSL.isNifti dwi = withTempDir $ \tmpdir -> do
--                       FSL.extractB0 dwi (tmpdir </> "b0.nii.gz")
--                       liftIO $ Util.convertImage (tmpdir </> "b0.nii.gz") out
--                   | Teem.isNrrd dwi = withTempDir $ \tmpdir -> do
--                       liftIO $ Teem.extractB0 dwi (tmpdir </> "b0.nrrd")
--                       liftIO $ Util.convertImage (tmpdir </> "b0.nrrd") out
--                   | otherwise = error $ "extractB0: Invalid dwi, must be nrrd or nifti: " ++ dwi
