{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}
module Node.MeasureTractsCsv
  ( rules
  , MeasureTractsCsv (..)
  )
  where

import           Node.DWI             hiding (rules)
import           Node.DWIMask         hiding (rules)
import           Node.FreeSurfer      hiding (rules)
import           Node.MeasureTracts   hiding (rules)
import           Node.TractQuerier    hiding (rules)
import           Node.UKFTractography hiding (rules)
import           Node.Util
import           Node.WmparcInDwi     hiding (rules)
import           Node.WmqlTracts      hiding (rules)
import           Paths                (outdir)
import           Shake.BuildNode
import           Util                 (convertImage)
import Data.List (intercalate)

type CaseId = String

data MeasureTractsCsv = MeasureTractsCsv {fstype      :: FreeSurferType
                                         ,fs2dwitype  :: FsToDwiType
                                         ,dwitype     :: DwiType
                                         ,dwimasktype :: DwiMaskType
                                         ,ukftype     :: UKFTractographyType
                                         ,caseid      :: CaseId }
                      deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)


instance BuildNode MeasureTractsCsv where
  path n@(MeasureTractsCsv{..}) = outdir </> caseid </> showKey n <.> "csv"

  build n@(MeasureTractsCsv {..}) = Just $ do
    bin <- getMeasureTracts
    let tracts = WmqlTracts {..}
        algo = "WmqlTracts-(" ++ (intercalate "," [show fstype,show fs2dwitype,show dwitype,show dwimasktype,show ukftype])
                ++ ")"
    need tracts
    vtks <- getDirectoryFiles ""  [(pathDir tracts) </> "*.vtk"]
    command_ [] (bin </> "measureTracts.py") $ 
      ["-f"
      ,"-c", "caseid", "algo"
      ,"-v", caseid, algo
      ,"-o", (path n)
      ,"-i"] ++  vtks

rules = rule (buildNode :: MeasureTractsCsv -> Maybe (Action [Double]))
