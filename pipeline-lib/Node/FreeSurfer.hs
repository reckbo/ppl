{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
module Node.FreeSurfer
  (FreeSurfer (..)
  , rules
  ) where

import           Control.Monad       (when)
import           Control.Monad.Extra (unlessM, whenM)
import           Data.List           (intercalate)
import           Data.List.Split
import           Data.Maybe
import           FSL                 (isNifti)
import           Node.Structural     hiding (rules)
import           Node.StructuralMask hiding (rules)
import           Node.Types
import           Node.Util
import           Shake.BuildNode
import qualified System.Directory    as IO
import           System.Environment  (getEnvironment, lookupEnv)
import           Util                (convertImage, maskImage)

data FreeSurfer = FreeSurfer { fstype :: FreeSurferType, caseid :: CaseId }
                   deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

instance BuildNode FreeSurfer where
  paths (FreeSurfer FreeSurferGiven caseid)
    = map (\f -> getPath "freesurfer" caseid </> f) [ "mri/brain.mgz"
                                                    , "mri/wmparc.mgz"]

  paths n@(FreeSurfer {..}) =
    [outdir </> caseid </> showKey n </> "mri/brain.mgz"
    ,outdir </> caseid </> showKey n </> "mri/wmparc.mgz"]

  build n@(FreeSurfer FreeSurferGiven _) = Nothing

  build out@(FreeSurfer (FreeSurferFromT1Given masktype) caseid) = Just $ do
    let strct = Structural T1w caseid
    let mask = StructuralMask masktype T1w caseid
    buildFromMask strct mask (path out)

  build out@(FreeSurfer (FreeSurferFromT1XC masktype) caseid) = Just $ do
    let strct = Structural (StructuralXC T1w) caseid
    let mask = StructuralMask masktype (StructuralXC T1w) caseid
    need mask
    need strct
    buildFromMask strct mask (path out)

buildFromMask :: Structural -> StructuralMask -> FilePath -> Action ()
buildFromMask strct mask out =
  runWithMask [5,3,0]
              (path mask)
              (path strct)
              (takeDirectory out)


rules = rule (buildNode :: FreeSurfer -> Maybe (Action [Double]))

--------------------------------------------------------------------------------
-- Functions

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

run :: Version -> Bool -> FilePath -> FilePath -> Action ()
run version skullstrip t1 outdir = withTempDir $ \tmpdir -> do
  liftIO $ unlessM (IO.doesFileExist t1)
    (error $ "Freesurfer: run: "++t1++" does not exist")
  fshome <- assertVersion version
  let caseid       = dropExtensions . takeBaseName $ t1
      subjectsDir  = tmpdir </> "subjects"
      fsdir        = subjectsDir </> caseid
      t1mgz        = fsdir </> "mri" </> "T1.mgz"
      brainmaskmgz = fsdir </> "mri" </> "brainmask.mgz"
  liftIO $ IO.createDirectoryIfMissing False subjectsDir
  t1nii <- liftIO $ if isNifti t1 then return t1
                    else (do Util.convertImage t1 (tmpdir </> "t1.nii.gz")
                             return (tmpdir </> "t1.nii.gz"))
  fscmd fshome subjectsDir "recon-all" $ ["-s", caseid ,"-i", t1nii ,"-autorecon1"]
    ++ (if skullstrip then [] else ["-noskullstrip"])
  liftIO $ IO.copyFile t1mgz brainmaskmgz
  fscmd fshome subjectsDir "recon-all" ["-autorecon2", "-subjid", caseid]
  fscmd fshome subjectsDir "recon-all" ["-autorecon3", "-subjid", caseid]
  liftIO $ whenM (IO.doesDirectoryExist outdir) (IO.removeDirectoryRecursive outdir)
  unit $ cmd "mv" fsdir outdir

runWithMask :: Version -> FilePath -> FilePath -> FilePath -> Action ()
runWithMask version mask t1 outdir = withTempDir $ \tmpdir -> do
    -- let maskedt1 = tmpdir </> "masked-" ++ (takeBaseName t1) -- T1w.nii no such file
    let maskedt1 = tmpdir </> "masked-t1.nii.gz" -- TODO get working for now
    liftIO $ Util.maskImage t1 mask maskedt1
    run version False maskedt1 outdir

fscmd :: FilePath -> FilePath -> FilePath -> [FilePath] -> Action ()
fscmd fshome subjdir exe args
  = cmd Shell "source" (fshome </> "SetUpFreeSurfer.sh")
    ("; export SUBJECTS_DIR=" ++ subjdir)
    ";" exe args
