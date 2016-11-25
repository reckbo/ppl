module Paths
  (outdir
  ,t1
  ,dwi
  ,dwimask
  ,sourceDwi
  ,t1MaskMabs
  ,freeSurfer
  -- software
  ,ukfTractographyExePrefix
  ,ukfTractographyDir
  ,tractQuerierPrefix
  ,antsPrefix
  )
  where

import           BuildNode.HCP.Types        (PhaseOrientation (..))
import           Development.Shake.FilePath (FilePath, (</>))
import           PathsOutputHCP
import           Text.Printf

--------------------------------------------------------------------------------
-- Source (ungenerated) data paths - modify these

t1 caseid = "in" </> caseid </> caseid ++ "_3T_T1w_MPR1.nii.gz"
dwi caseid = "in" </> caseid </> caseid ++ "-dwi.nrrd"
dwimask caseid = "in" </> caseid </> caseid ++ "-dwi-tensor-mask.nrrd"
-- hcp
sourceDwi Pos num caseid = printf "in/%s.dwiPA%d.nii.gz" caseid num
sourceDwi Neg num caseid = printf "in/%s.dwiAP%d.nii.gz" caseid num

--------------------------------------------------------------------------------
-- Generated Data Path (defaults should be fine)

outdir = "_data"
t1MaskMabs caseid = outdir </> caseid </> caseid ++ "-" ++ "mabsT1Mask.nii.gz"
freeSurfer caseid = outdir </> caseid </> caseid ++ "-" ++ "freesurfer"
ukfTractographyExePrefix = outdir </> "UKFTractography"
ukfTractographyDir caseid = outdir </> caseid
tractQuerierPrefix = outdir </> "tract_querier"
antsPrefix = outdir </> "ANTs"
