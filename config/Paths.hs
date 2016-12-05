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
  -- ,freeSurferGiven
  ,hcpdir
  -- software
  ,ukfTractographyExePrefix
  ,ukfTractographyDir
  ,tractQuerierPrefix
  ,antsPrefix
  )
  where

import           Data.List                  (intercalate)
import           Development.Shake.FilePath (FilePath, (<.>), (</>))
import           Pipeline.HCP.Types         (PhaseOrientation (..))
import           Text.Printf

--------------------------------------------------------------------------------
-- Source (ungenerated) data paths - modify these

diff caseid x = "in" </> caseid </> "diff" </> caseid ++ "-" ++ x
strct caseid x = "in" </> caseid </> "strct" </> caseid ++ "-" ++ x

dwi caseid = Just $ diff caseid "dwi-Ed.nhdr"
dwimask caseid = Just $ diff caseid "tensor-mask.nhdr"
t1 caseid = Nothing
t2 caseid = Nothing
freeSurfer caseid = strct caseid "freesurfer"
t1mask caseid = Nothing
t2mask caseid = Nothing
dwiHcp _ _ _ = Nothing

--------------------------------------------------------------------------------
-- Generated Data Path (defaults should be fine)

outdir = "_data"
t1mabs caseid = outdir </> caseid </> caseid ++ "-" ++ "mabsT1Mask.nii.gz"
t2mabs caseid = outdir </> caseid </> caseid ++ "-" ++ "mabsT2Mask.nii.gz"
t1rigidmabs caseid = outdir </> caseid </> caseid ++ "-" ++ "rigid-mabs.nii.gz"
t2rigidmabs caseid = outdir </> caseid </> caseid ++ "-" ++ "rigid-mabs.nii.gz"
t1rigidmask caseid = outdir </> caseid </> caseid ++ "-" ++ "rigid-mask.nii.gz"
t2rigidmask caseid = outdir </> caseid </> caseid ++ "-" ++ "rigid-mask.nii.gz"
-- freeSurfer caseid = outdir </> caseid </> caseid ++ "-" ++ "freesurfer"
fsInDwiDir caseid = outdir </> caseid
ukfTractographyExePrefix = outdir </> "UKFTractography"
ukfTractographyDir caseid = outdir </> caseid
tractQuerierPrefix = outdir </> "tract_querier"
antsPrefix = outdir </> "ANTs"

showIndices :: [Int] -> String
showIndices = intercalate "-" . map show

hcpdir :: String -> FilePath -> FilePath
hcpdir caseid stage = outdir </> caseid </> "hcp" </> stage
