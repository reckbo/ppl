module Need where

import           Node

fsTypes      = [FreeSurferWithMask StructuralMaskMabs]
fs2dwiTypes  = [FsBrain_B0]
dwiTypes     = [DwiGiven, DwiHcp [98,99]]
dwimaskTypes = [DwiMaskHcp (DwiHcp [98,99])]
ukfTypes     = [UKFTractographyDefault]

-- makeSetUpData = do
--   let       map

-- need :: String -> Action ()
-- need caseid = do
  -- let node = fromJust . lookup "measuretracts" $ nodeByCaseid
               -- {fstype=fs
               -- ,fs2dwitype=fs2dwi
               -- ,dwitype=dwi
               -- ,dwimasktype=dwimask
               -- ,ukftype=ukf
               -- ,caseids=caseids}
  {-let node = MeasureTractsAllCsv-}
               {-{fstype=FreeSurferGiven-}
               {-,fs2dwitype=FsBrain_B0-}
               {-,dwitype=DwiGiven-}
               {-,dwimasktype=DwiMaskGiven-}
               {-,ukftype=UKFTractographyDefault-}
               {-,caseids=caseids}-}
  -- Shake.BuildNode.need node
  -- return ()
