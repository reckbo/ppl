module Paths
  (outdir
  ,t1
  ,t2
  ,t1mask
  ,t2mask
  ,dwi
  ,dwimask
  ,dwiHcp
  ,freeSurfer
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
import           Node.HCP.Types         (PhaseOrientation (..))
import           Text.Printf

--------------------------------------------------------------------------------
-- Source (ungenerated) data paths - modify these

pre caseid x = "in" </> caseid ++ x
{-strct caseid x = "in" </> caseid </> "strct" </> caseid ++ "." ++ x-}

dwi caseid = Just $ pre caseid ".dwi-Ed.nrrd"
dwimask caseid = Just $ pre caseid "-tensor-mask.nrrd"
freeSurfer caseid = Just $ pre caseid ".freesurfer"
t1 caseid = Nothing
t2 caseid = Nothing
t1mask caseid = Nothing
t2mask caseid = Nothing
dwiHcp _ _ _ = Nothing

--------------------------------------------------------------------------------
-- Generated Data Path (defaults should be fine)

outdir = "_data"
ukfTractographyExePrefix = outdir </> "UKFTractography"
ukfTractographyDir caseid = outdir </> caseid
tractQuerierPrefix = outdir </> "tract_querier"
antsPrefix = outdir </> "ANTs"

hcpdir :: String -> FilePath -> FilePath
hcpdir caseid stage = outdir </> caseid </> "hcp" </> stage
