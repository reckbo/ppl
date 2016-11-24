module FreeSurfer
(run
,runWithMask
) where

import           Control.Monad            (when)
import           Data.List                (intercalate)
import           Data.List.Split
import           Data.Maybe               (fromMaybe)
import           Development.Shake.Config
import           FSL                      (isNifti)
import           Shake.BuildNode
import qualified System.Directory         as IO (copyFile, doesFileExist,
                                                 renameFile)
import           System.Environment       (getEnvironment, lookupEnv)
import           System.IO.Temp           (withSystemTempDirectory,
                                           withSystemTempFile)
import           System.Process           (CreateProcess (..), callProcess,
                                           createProcess, proc)
import           Util                     (convertImage, maskImage)

type Version = [Int]

assertVersion :: Version -> Action FilePath
assertVersion requiredVersion =
  let showVersion = intercalate "." . map show
      extractVersion = map read . splitOn "." . tail . head . reverse . splitOn "-"
  in do
    fshome <- liftIO $ (fromMaybe $ error "Set FREESURFER_HOME") <$> lookupEnv "FREESURFER_HOME"
    let versionFile = fshome </> "build-stamp.txt"
    exists <- liftIO $ IO.doesFileExist versionFile
    when (not exists)
      (error $ "Can't check Freesurfer version, "
       ++ fshome ++ "/build-stamp.txt doesn't exist. Is that directory correct?")
    version <- liftIO $ extractVersion <$> readFile versionFile
    when (version /= requiredVersion) $
        error $ "Freesurfer version " ++ (showVersion version)
                ++ " detected, but require " ++ (showVersion requiredVersion)
    return fshome


runCmd :: FilePath -> FilePath -> FilePath -> [FilePath] -> Action ()
runCmd fshome subjdir exe args = cmd (AddEnv "SUBJECTS_DIR" subjdir) exe args

run :: Version -> Bool -> FilePath -> FilePath -> Action ()
run version skullstrip t1 outdir = withTempDir $ \tmpdir -> do
  fshome <- assertVersion version
  let t1nii = tmpdir </> "t1.nii.gz"
      caseid = dropExtensions . takeBaseName $ t1
      subjectsDir = tmpdir </> "subjects"
      fsdir = subjectsDir </> caseid
      t1mgz = tmpdir </> caseid </> "mri" </> "T1.mgz"
      brainmaskmgz = tmpdir </> caseid </> "mri" </> "brainmask.mgz"
  liftIO $ convertImage t1 t1nii
  runCmd fshome subjectsDir "recon-all" $ ["-s", caseid ,"-i", t1nii ,"-autorecon1"]
    ++ (if skullstrip then [] else ["-noskullstrip"])
  liftIO $ IO.copyFile t1mgz brainmaskmgz
  runCmd fshome subjectsDir "recon-all" ["-autorecon2", "-subjid", caseid]
  runCmd fshome subjectsDir "recon-all" ["-autorecon3", "-subjid", caseid]
  liftIO $ IO.renameFile fsdir outdir

runWithMask :: Version -> FilePath -> FilePath -> FilePath -> Action ()
runWithMask version mask t1 outdir = withTempDir $ \tmpdir -> do
    let maskedt1 = tmpdir </> "maskedt1ForFreesurfer.nii.gz"
    liftIO $ Util.maskImage t1 mask maskedt1
    run version False maskedt1 outdir
