module InputPaths
  (t1
  ) where

import Development.Shake.FilePath ((</>))

t1 caseid = "in" </> caseid </> caseid ++ "_3T_T1w_MPR1.nii.gz"

-- t1Training =
-- t1TrainingMasks =
-- t1Training_caselist = 