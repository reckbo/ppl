{-# LANGUAGE RecordWildCards #-}
module Need where

import           Node
import           Node.TractMeasures
import           Node.Types
import           Node.WmqlTracts
import           Shake.BuildNode    (path, (</>))


tractMeasuresFromCaseid caseid =
  [TractMeasures{..}
  |fstype <- [FreeSurferFromT1XC (StructuralMaskMabs bthash)]
  ,fs2dwitype <- [FsBrain_B0]
  ,dwitype <- [DwiXC DwiGiven,DwiXC DwiGiven]
  ,dwimasktype <- [DwiMaskGiven]
  ,ukftype <- [UKFTractographyDefault]]
  where tqhash = "a8e354e"
        bthash = "e13c873"
        ukfhash = "2dbede4"


fsInDwiFromCaseid caseid  = []
  --  =
  -- [FsInDwi bthash fs2dwitype fstype dwitype dwimasktype caseid
  -- |fs2dwitype <- [FsBrain_B0]
  -- ,fstype <- [FreeSurferFromT1XC (StructuralMaskMabs bthash)]
  -- ,dwitype <- [DwiXC DwiGiven,DwiXC (DwiHcp [98,99])]
  -- ,dwimasktype <- [DwiMaskHcp]]
  -- where bthash = "e13c873"

ukfFromCaseid caseid = []
  -- [UKFTractography (ukfT-- ype, dwiType, dwimaskType, subjid)
  -- | ukfType <- [UKFTractographyDefault]
  -- , dwiType <- [DwiXC DwiGiven]
  -- , dwimaskType <- [DwiMaskHcp]
  -- ]

wmqlFromCaseid caseid = []-- [ WmqlTracts fsType fs2 dwiType dwiType dwimaskType ukfType subjid
                          -- | fs2dwiType  <- [FsBrain_B0]
                          -- , fsType      <- [FreeSurferFromT1Given StructuralMaskMabs]
                          -- , dwiType     <- [DwiGiven]
                          -- , dwimaskType <- [DwiMaskHcp]
                          -- , ukfType     <- [UKFTractographyDefault]
                          -- ]
