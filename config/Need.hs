module Need where

import           Node
import           Node.MeasureTractsAllCsv hiding (rules)
import           Shake.BuildNode

need :: [String] -> Action ()
need caseids = do
  {-let node = MeasureTractsAllCsv-}
               {-{fstype=FreeSurferWithMask StructuralMaskMabs-}
               {-,fs2dwitype=FsBrain_B0-}
               {-,dwitype=DwiGiven-}
               {-,dwimasktype=DwiMaskGiven-}
               {-,ukftype=UKFTractographyDefault-}
               {-,caseids=caseids}-}
  let node = MeasureTractsAllCsv
               {fstype=FreeSurferGiven
               ,fs2dwitype=FsBrain_B0
               ,dwitype=DwiGiven
               ,dwimasktype=DwiMaskGiven
               ,ukftype=UKFTractographyDefault
               ,caseids=caseids}
  Shake.BuildNode.need node
  return ()
