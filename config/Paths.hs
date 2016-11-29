module Paths
  (outdir
  ,t1
  ,t2
  ,t1mask
  ,t2mask
  ,dwi
  ,dwimask
  ,fsInDwiDir
  ,dwiHcp
  ,t1mabs
  ,t2mabs
  ,t1rigidmabs
  ,t2rigidmabs
  ,t1rigidmask
  ,t2rigidmask
  ,freeSurfer
  ,hcpdir
  -- software
  ,ukfTractographyExePrefix
  ,ukfTractographyDir
  ,tractQuerierPrefix
  ,antsPrefix
  )
  where

import           Development.Shake.FilePath (FilePath, (</>), (<.>))
import           Pipeline.HCP.Types         (PhaseOrientation (..))
import           Text.Printf
import Data.List (intercalate)

--------------------------------------------------------------------------------
-- Source (ungenerated) data paths - modify these

t1 caseid = "in" </> caseid </> caseid ++ "_3T_T1w_MPR1.nii.gz"
t2 caseid = "in" </> caseid </> caseid ++ "_3T_T2w_MPR1.nii.gz"
t1mask caseid = "in" </> caseid </> caseid ++ "-mabsT1Mask.nii.gz"
t2mask caseid = "in" </> caseid </> caseid ++ "_3T_T2w_MPR1-mask.nii.gz"
-- dwi caseid = "in" </> caseid </> caseid ++ "-dwi.nrrd"
dwi caseid = "in" </> caseid </> "data-1" <.> "nii.gz"
dwimask caseid = "in" </> caseid </> caseid ++ "-dwimask" <.> "nii.gz"
-- hcp
dwiHcp Pos num caseid = "in" </> caseid </> (show num) ++ "PA" <.> "nii.gz"
dwiHcp Neg num caseid = "in" </> caseid </> (show num) ++ "AP" <.> "nii.gz"

--------------------------------------------------------------------------------
-- Generated Data Path (defaults should be fine)

outdir = "_data"
t1mabs caseid = outdir </> caseid </> caseid ++ "-" ++ "mabsT1Mask.nii.gz"
t2mabs caseid = outdir </> caseid </> caseid ++ "-" ++ "mabsT2Mask.nii.gz"
t1rigidmabs caseid = outdir </> caseid </> caseid ++ "-" ++ "rigid-mabs.nii.gz"
t2rigidmabs caseid = outdir </> caseid </> caseid ++ "-" ++ "rigid-mabs.nii.gz"
t1rigidmask caseid = outdir </> caseid </> caseid ++ "-" ++ "rigid-mask.nii.gz"
t2rigidmask caseid = outdir </> caseid </> caseid ++ "-" ++ "rigid-mask.nii.gz"
freeSurfer caseid = outdir </> caseid </> caseid ++ "-" ++ "freesurfer"
fsInDwiDir caseid = outdir </> caseid
ukfTractographyExePrefix = outdir </> "UKFTractography"
ukfTractographyDir caseid = outdir </> caseid
tractQuerierPrefix = outdir </> "tract_querier"
antsPrefix = outdir </> "ANTs"

showIndices :: [Int] -> String
showIndices = intercalate "-" . map show

hcpdir :: String -> FilePath -> FilePath
hcpdir caseid stage = outdir </> caseid </> "hcp" </> stage

