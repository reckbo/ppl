{-# LANGUAGE RecordWildCards #-}
module Need where

import           Node
import           Node.MeasureTractsCsv
import           Node.WmqlTracts
import           Shake.BuildNode       (path, (</>))


measureTractsFromCaseid subjid =
    [ MeasureTractsCsv fs fs2dwi dwi dwimask ukf subjid
    | fs2dwi <- [FsBrain_B0]
    , fs <- [FreeSurferFromT1XC StructuralMaskMabs]
    , dwi <- [DwiXC DwiGiven, DwiXC (DwiHcp [98, 99])]
    , dwimask <- [DwiMaskHcp]
    , ukf <- [UKFTractographyDefault] ]

wmparcInDwiFromCaseid subjid   -- []
 =
    [ WmparcInDwi (fs2dwiType, fsType, dwiType, dwimaskType, subjid)
    | fs2dwiType <- [FsBrain_B0]
    , fsType <- [FreeSurferFromT1XC StructuralMaskMabs]
    , dwiType <- [DwiXC DwiGiven, DwiXC (DwiHcp [98, 99])]
    , dwimaskType <- [DwiMaskHcp] ]

ukfFromCaseid subjid = []
  -- [UKFTractography (ukfT-- ype, dwiType, dwimaskType, subjid)
  -- | ukfType <- [UKFTractographyDefault]
  -- , dwiType <- [DwiXC DwiGiven]
  -- , dwimaskType <- [DwiMaskHcp]
  -- ]

wmqlFromCaseid subjid = []-- [ WmqlTracts fsType fs2 dwiType dwiType dwimaskType ukfType subjid
                          -- | fs2dwiType  <- [FsBrain_B0]
                          -- , fsType      <- [FreeSurferFromT1Given StructuralMaskMabs]
                          -- , dwiType     <- [DwiGiven]
                          -- , dwimaskType <- [DwiMaskHcp]
                          -- , ukfType     <- [UKFTractographyDefault]
                          -- ]

pathsMeasureTracts :: FilePath
                   -> Int
                   -> MeasureTractsCsv
                   -> [(String, FilePath)]
pathsMeasureTracts projdir idx n@(MeasureTractsCsv{..}) =
    let wmql =
            WmqlTracts {..}
    in [("measuretracts" ++ show idx, projdir </> path n)] ++
       pathsWmql projdir idx wmql

pathsWmql :: FilePath -> Int -> WmqlTracts -> [(String, FilePath)]
pathsWmql projdir idx n = [("wmql" ++ show idx, projdir </> path n)]
