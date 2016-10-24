{-# LANGUAGE FlexibleInstances #-}
module HCP.Config
  ( outdir
  -- Normalized
  , sourceDwi_path
  , normalizedDwi_path
  , b0sPairsYaml_path
  , meanB0_path
  -- Preprocessing
  , acqParams_path
  , posNegVol_path
  ) where

import           Shake.BuildKey
import           FSL         (BValue (..), FslDwi (..))
import           HCP.Types
import           Text.Printf

-----------------------------------------------------------------------
-- Input Paths

sourceDwi_path :: PhaseOrientation -> Int -> CaseId -> FilePath
sourceDwi_path Pos num caseid = printf "src/%s.dwiPA%d.nii.gz" caseid num
sourceDwi_path Neg num caseid = printf "src/%s.dwiAP%d.nii.gz" caseid num

-----------------------------------------------------------------------
-- Output Directory

outdir :: FilePath
outdir = "_data"

-----------------------------------------------------------------------
-- Preprocessing Paths

acqParams_path caseid = outdir </> caseid </> "hcp/1_Preprocessing" </> "acqparams.txt"

posNegVol_path caseid = outdir </> caseid </> "hcp/1_Preprocessing" </> "PosNeg.nii.gz"


-----------------------------------------------------------------------
-- Normalization Paths

normalizedDwi_path :: PhaseOrientation -> Int -> CaseId -> FilePath
normalizedDwi_path phasedir num caseid =
  -- [qc|{outdir}/{caseid}/hcp/0_normalized/{phasedir}-{num}.nii.gz|]
    foldr (</>) ""
    [outdir
    ,caseid
    ,"hcp/0_normalized"
    ,printf "%s-%i.nii.gz" (show phasedir) num
    ]

b0sPairsYaml_path caseid =
  -- [qq|{outdir}/$caseid/hcp/0_normalized/dwipairs.yaml|]
    foldr (</>) ""
    [outdir
    ,caseid
    ,"hcp/0_normalized/b0sPairs.yaml"]

meanB0_path caseid =
  -- [qc|{outdir}/{caseid}/hcp/0_normalized/Pos-1-meanb0|]
    foldr (</>) ""
    [outdir
    ,caseid
    ,"hcp/0_normalized/Pos-1-meanb0.txt"]
