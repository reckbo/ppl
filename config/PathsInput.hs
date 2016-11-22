module PathsInput
  (t1
  ,sourceDwi_path
  ) where

import           Development.Shake.FilePath ((</>))
import           HCP.Types
import           Text.Printf

t1 caseid = "in" </> caseid </> caseid ++ "_3T_T1w_MPR1.nii.gz"

--------------------------------------------------------------------------------
-- HCP
sourceDwi_path Pos num caseid = printf "in/%s.dwiPA%d.nii.gz" caseid num
sourceDwi_path Neg num caseid = printf "in/%s.dwiAP%d.nii.gz" caseid num
