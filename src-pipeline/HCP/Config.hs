{-# LANGUAGE FlexibleInstances          #-}
module HCP.Config
  ( b0maxbval
  , b0dist
  , getSourcePosDwis
  , getSourceNegDwis
  , dwiPairsYaml_path
  , meanB0_path
  ) where

import PNLPipeline
import Text.Printf
import Data.List
import FSL (FslDwi(..), BValue (..), tobval, tobvec)
import HCP.DwiTypes

b0maxbval :: BValue
b0maxbval = BValue 50

b0dist :: Int
b0dist = 45

outdir = "_data"

type CaseId = String

posDwis = ["src/{case}.dwiPA1.nii.gz","src/{case}.dwiPA2.nii.gz"]
negDwis = ["src/{case}.dwiAP1.nii.gz","src/{case}.dwiAP2.nii.gz"]
phaseDir = PA
echoSpacing = 0.20

getSourcePosDwis caseid = map (flip withCaseId caseid) posDwis
getSourceNegDwis caseid = map (flip withCaseId caseid) negDwis

-----------------------------------------------------------------------
-- Normalization

dwiPairsYaml_path caseid = foldr (</>) ""
  [outdir,caseid,"hcp","0_normalized","dwipairs.yaml"]

meanB0_path caseid = foldr (</>) ""
  [outdir, caseid, "hcp", "0_normalized", "Pos-1-meanb0"]

instance FslDwi HcpDwi where
  nifti (HcpDwi dwitype direction num caseid) = case dwitype of
    SourceDwi -> "src" </> concat [caseid,".",show direction, show num,".nii.gz"]
    NormalizedDwi -> foldr (</>) ""
      [outdir,caseid,"0_normalized",concat [show direction, "-",show num,".nii.gz"]]
  bvec = tobvec . nifti
  bval = tobval . nifti
