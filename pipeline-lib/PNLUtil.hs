module PNLUtil
  (convertImage
  ,maskImage
  ) where

import           Development.Shake.FilePath (takeExtensions, (<.>))
import qualified FSL                        (isNifti, mask)
import qualified Nrrd                       (isNrrd, mask)
import qualified System.Directory           as IO (copyFile)
import           System.IO.Temp             (withSystemTempFile)
import           System.Process             (callProcess)

-- import           Data.List                  (intercalate)
-- import           Data.List.Split            (splitOn)
-- withCaseId :: String -> FilePath -> FilePath
-- withCaseId caseid = intercalate caseid . splitOn "{case}"

convertImage :: FilePath -> FilePath -> IO ()
convertImage infile outfile
  = if takeExtensions infile == takeExtensions outfile then
      IO.copyFile infile outfile
    else
      callProcess "ConvertBetweenFileFormats" [infile, outfile]

maskImage :: FilePath -> FilePath -> FilePath -> IO ()
maskImage mask img out | FSL.isNifti out = maskImageUsing "nii.gz" FSL.mask out
                       | Nrrd.isNrrd out = maskImageUsing "nrrd" Nrrd.mask out
                       | otherwise = error "maskImage: images must be in Nrrd or Nifti format"
                       where maskImageUsing ext maskFn out
                               = withSystemTempFile ("m" <.> ext) $ \tmpmask _ -> do
                               withSystemTempFile ("i" <.> ext)$ \tmpimg _ -> do
                                 maskFn tmpimg tmpmask out
