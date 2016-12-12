{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
module Node.MeasureTractsCsv
  ( rules
  , MeasureTractsCsv (..)
  )
  where

import           Node.DWI             hiding (rules)
import           Node.DWIMask         hiding (rules)
import           Node.FreeSurfer      hiding (rules)
import           Node.TractQuerier    hiding (rules)
import           Node.UKFTractography hiding (rules)
import           Node.WmparcInDwi hiding (rules)
import           Node.WmqlTracts hiding (rules)
import           Node.Util
import           Node.MeasureTracts     hiding (rules)
import           Shake.BuildNode
import           Util                 (convertImage)

type CaseId = String

data MeasureTractsCsv = MeasureTractsCsv {fstype :: FreeSurferType
                                         ,fs2dwitype :: FsToDwiType
                                         ,dwitype :: DwiType
                                         ,dwimasktype :: DwiMaskType
                                         ,ukftype :: UKFTractographyType
                                         ,caseid :: CaseId }
                      deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)


instance BuildNode MeasureTractsCsv where
  -- path n@(WmqlTracts (_,_,_,_,_,caseid)) = outdir </> caseid </> showKey n </> "stamp.txt"
  path n@(MeasureTractsCsv _ _ _ _ _ caseid) = outdir </> caseid </> showKey n <.> "csv"

  build n@(MeasureTractsCsv {..}) = Just $ do
    bin <- getMeasureTracts
    let tracts = WmqlTracts {..}
    need tracts
    vtks <- getDirectoryFiles ""  [(pathDir tracts) </> "*.vtk"]
    unit $ cmd (bin </> "measureTracts.py")
      "-f"
      "-i" vtks
      "-o" (path n)

rules = rule (buildNode :: MeasureTractsCsv -> Maybe (Action [Double]))
