module HcpInputPaths
  ( sourceDwi_path
  ) where

import           Shake.BuildNode
import           HCP.Types
import           Text.Printf

sourceDwi_path :: PhaseOrientation -> Int -> CaseId -> FilePath
sourceDwi_path Pos num caseid = printf "in/%s.dwiPA%d.nii.gz" caseid num

sourceDwi_path Neg num caseid = printf "in/%s.dwiAP%d.nii.gz" caseid num
