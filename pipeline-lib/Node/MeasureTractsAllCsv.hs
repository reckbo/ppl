{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}
module Node.MeasureTractsAllCsv
  ( rules
  , MeasureTractsAllCsv (..)
  )
  where

import           Node.DWI              hiding (rules)
import           Node.DWIMask          hiding (rules)
import           Node.FreeSurfer       hiding (rules)
import           Node.MeasureTractsCsv hiding (rules)
import           Node.TractQuerier     hiding (rules)
import           Node.UKFTractography  hiding (rules)
import           Node.WmparcInDwi      hiding (rules)
import           Node.WmqlTracts       hiding (rules)
import           Node.Util
import           Shake.BuildNode

type CaseId = String

data MeasureTractsAllCsv = MeasureTractsAllCsv {fstype      :: FreeSurferType
                                               ,fs2dwitype  :: FsToDwiType
                                               ,dwitype     :: DwiType
                                               ,dwimasktype :: DwiMaskType
                                               ,ukftype     :: UKFTractographyType
                                               ,caseids     :: [CaseId] }
                      deriving (Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

instance Show MeasureTractsAllCsv where
  show (MeasureTractsAllCsv{..}) =
    concat ["MeasureTractsAllCsv "
           ,"("
           ,show fstype, ","
           ,show fs2dwitype, ","
           ,show dwitype, ","
           ,show dwimasktype, ","
           ,show ukftype
           ,")"]

instance BuildNode MeasureTractsAllCsv where
  path n = outdir </> showKey n <.> "csv"

  build n@(MeasureTractsAllCsv {..}) = Just $ do
    let measures = [MeasureTractsCsv {..} | caseid <- caseids]
    needs measures
    -- TODO get rid of csvkit dependency
    if length measures == 1 then
        copyFile' (path . head $ measures) (path n)
    else do
        Stdout out <- cmd "csvstack" (map path measures)
        writeFile' (path n) out

rules = rule (buildNode :: MeasureTractsAllCsv -> Maybe (Action [Double]))
