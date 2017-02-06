{-# LANGUAGE RecordWildCards #-}
module Need where

import           Node
import           Node.TractMeasures
import           Node.Types
import           Node.WmqlTracts
import           Node.Dwi
import           Shake.BuildNode    (path, (</>))

bthashDefault = "e13c873"

dwiFromCaseid caseid = 
 [Dwi{..}
 | dwitype <- [DwiEpi DwiGiven DwiMaskHcp T2wGiven (NormalMask $ StructuralMaskMabs bthash) bthash]
 ]
  where bthash = bthashDefault


tractMeasuresFromCaseid caseid = []
  --[TractMeasures{..}
  -- |fstype <- [FreeSurferUsingMask T1wXc (NormalMask (StructuralMaskMabs bthash))]
--  ,fs2dwimethod <- [FsBrain_B0]
--  ,dwitype <- [DwiXC DwiGiven]
--  ,dwimaskmethod <- [DwiMaskGiven]
--  ,ukftype <- [UKFTractographyDefault]]
--  where tqhash = "a8e354e"
--        bthash = "e13c873"
 --       ukfhash = "999f14d"


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
