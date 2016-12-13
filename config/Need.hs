module Need where

import           Node
import           Node.MeasureTractsCsv hiding (rules)
import           Shake.BuildNode

need :: [String] -> Action ()
need caseids = do
  let nodes = [MeasureTractsCsv
               {fstype=FreeSurferWithMask StructuralMaskMabs
               ,fs2dwitype=FsBrain_B0
               ,dwitype=DwiGiven
               ,dwimasktype=DwiMaskGiven
               ,ukftype=UKFTractographyDefault
               ,caseid=caseid}
              | caseid <- caseids]
  needs nodes
  return ()
