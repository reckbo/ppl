{-# LANGUAGE FlexibleInstances #-}
module HCP.Config
  ( outdir
  , phaseDirection
  , b0maxbval
  , b0dist
  , numDwiPairs
  , sourceDwi_path
  , normalizedDwi_path
  , dwiPairsYaml_path
  , meanB0_path
  ) where

import           Shake.BuildKey
import           FSL         (BValue (..), FslDwi (..))
import           HCP.Types
import           Text.Printf

-----------------------------------------------------------------------
-- Input Data Config

numDwiPairs :: Int
numDwiPairs = 2

phaseDirection = PA

echoSpacing = 0.20

b0maxbval :: BValue
b0maxbval = BValue 50

b0dist :: Int
b0dist = 45

sourceDwi_path :: PhaseDirection -> Int -> CaseId -> FilePath
sourceDwi_path phasedir num caseid =
  -- [qc|src/{caseid}.dwi{phasedir}{num}.nii.gz|]
      printf "src/%s.dwi%s%d.nii.gz" caseid (show phasedir) num


-----------------------------------------------------------------------
-- Output Config

-- Root output directory
outdir :: FilePath
outdir = "_data"

-- Normalization

normalizedDwi_path :: PhaseDirection -> Int -> CaseId -> FilePath
normalizedDwi_path phasedir num caseid =
  -- [qc|{outdir}/{caseid}/hcp/0_normalized/{phasedir}-{num}.nii.gz|]
    foldr (</>) ""
    [outdir
    ,caseid
    ,"hcp/0_normalized"
    ,printf "%s-%i.nii.gz" (show phasedir) num
    ]

dwiPairsYaml_path caseid =
  -- [qq|{outdir}/$caseid/hcp/0_normalized/dwipairs.yaml|]
    foldr (</>) ""
    [outdir
    ,caseid
    ,"hcp/0_normalized/dwipairs.yaml"]

meanB0_path caseid =
  -- [qc|{outdir}/{caseid}/hcp/0_normalized/Pos-1-meanb0|]
    foldr (</>) ""
    [outdir
    ,caseid
    ,"hcp/0_normalized/Pos-1-meanb0.yaml"]
