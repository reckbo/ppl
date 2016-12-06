module Paths where

import           Development.Shake.FilePath (FilePath, (<.>), (</>))
import           Node.HCP.Types             (PhaseOrientation (..))


given = [("dwi", "in/{case}/{case}.dwi-Ed.nrrd")
        ,("dwimask", "in/{case}/{case}-tensor-mask.nrrd")
        ,("freesurfer", "in/{case}/{case}.freesurfer")
        ]

-- dwi caseid = Just $ pre caseid ".dwi-Ed.nrrd"
-- dwimask caseid = Just $ pre caseid "-tensor-mask.nrrd"
-- freeSurfer caseid = Just $ pre caseid ".freesurfer"
-- t1 caseid = Nothing
-- t2 caseid = Nothing
-- t1mask caseid = Nothing
-- t2mask caseid = Nothing
-- dwiHcp _ _ _ = Nothing

outdir = "_data"
