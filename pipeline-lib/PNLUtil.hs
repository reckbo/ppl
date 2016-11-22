module PNLUtil
  ( withCaseId
  , convertFile
  ) where

import           Data.List                  (intercalate)
import           Data.List.Split            (splitOn)
import           Development.Shake.FilePath (takeExtensions)
import qualified FSL                        (isNifti, mask)
import qualified Nrrd                       (isNrrd, mask)
import qualified System.Directory           as IO (copyFile)
import           System.Process             (callProcess)

withCaseId :: String -> FilePath -> FilePath
withCaseId caseid = intercalate caseid . splitOn "{case}"

convertFile :: FilePath -> FilePath -> IO ()
convertFile infile outfile
  = if takeExtensions infile == takeExtensions outfile then
      IO.copyFile infile outfile
    else
      callProcess "ConvertBetweenFileFormats" [infile, outfile]

-- maskImage :: FilePath -> FilePath -> FilePath -> IO ()
-- maskImage mask img out | Nrrd.isNrrd mask && Nrrd.isNrrd img = Nrrd.mask mask img out
--                        | FSL.isNifti mask && FSL.isNifti img = FSL.mask mask img out
--                          -- 
-- getConfigWithCaseId :: String -> String -> Action [FilePath]
-- getConfigWithCaseId key caseid = do
--   key' <- getConfig key
--   case key' of
--     Nothing -> error $ "Missing key in config file: " ++ key
--     Just x -> return $ map (withCaseId caseid) . words $ x
