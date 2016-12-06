module Paths where

import           Development.Shake.FilePath (FilePath, (<.>), (</>))
import           Node.HCP.Types             (PhaseOrientation (..))


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

outdir = "_data"
