module Freesurfer
(
) where

import           Control.Monad            (when)
import           Data.List.Split
import           Development.Shake.Config
import           FSL                      (isNifti)
import           PNLUtil                  (convertImg, maskImage)
import           Shake.BuildNode
import qualified System.Directory         as IO (copyFile, renameFile)
import           System.Environment       (lookupEnv)

checkVersion :: Action ()
checkVersion = do
  Just required_version <- getConfig "freesurfer-version"
  fshome <- liftIO $ (maybe (error "Set FREESURFER_HOME") id) <$> lookupEnv "FREESURFER_HOME"
  buildstamp <- liftIO $ fmap (head . lines) $ readFile $ fshome </> "build-stamp.txt"
  let version = tail . head . reverse $ splitOn "-" buildstamp
  when (version /= required_version) $
    error $ "Freesurfer version " ++ version ++ " detected, but require "
    ++ required_version

run :: Bool -> FilePath -> FilePath -> Action ()
run skullstrip t1 outdir = withTempDir $ \tmpdir -> do
  checkVersion
  let t1nii = tmpdir </> "t1.nii.gz"
      subjectsDir = tmpdir </> "subjects"
      caseid = dropExtensions . takeBaseName $ t1
      fsdir = subjectsDir </> caseid
      t1mgz = tmpdir </> caseid </> "mri" </> "T1.mgz"
      brainmaskmgz = tmpdir </> caseid </> "mri" </> "brainmask.mgz"
  liftIO $ convertImg t1 t1nii
  command_ [AddEnv "SUBJECTS_DIR" subjectsDir]
    "recon-all" $ ["-s", caseid ,"-i", t1nii ,"-autorecon1"]
    ++ (if skullstrip then [] else ["-noskullstrip"])
  liftIO $ IO.copyFile t1mgz brainmaskmgz
  cmd [AddEnv "SUBJECTS_DIR" subjectsDir]
    "recon-all" "-autorecon2" "-subjid" caseid :: Action ()
  cmd [AddEnv "SUBJECTS_DIR" subjectsDir]
    "recon-all" "-autorecon3" "-subjid" caseid :: Action ()
  liftIO $ IO.renameFile fsdir outdir

runWithMask :: FilePath -> FilePath -> FilePath -> Action ()
runWithMask mask t1 outdir = withTempDir $ \tmpdir -> do
  liftIO $ PNLUtil.maskImage t1 mask (tmpdir </> "maskedt1.nii.gz")
  run False (tmpdir </> "maskedt1.nii.gz") outdir
