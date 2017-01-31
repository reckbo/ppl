{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}
module Node.TractMeasures
  (TractMeasures(..)
  ,rules)
  where

import           Data.List          (intercalate)
import           Node.Types
import           Node.Util
import           Node.WmqlTracts    hiding (rules)
import           Paths              (outdir)
import           Shake.BuildNode


data TractMeasures =
  TractMeasures {tqhash :: GitHash
                ,bthash :: GitHash
                ,fstype :: FreeSurferType
                ,fs2dwitype :: FsToDwiType
                ,dwitype :: DwiType
                ,dwimasktype :: DwiMaskType
                ,ukfhash :: GitHash
                ,ukftype :: UKFTractographyType
                ,caseid :: CaseId}
  deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)


instance BuildNode TractMeasures where
  path n@(TractMeasures{..}) = outdir </> caseid </> showKey n <.> "csv"
  build n@(TractMeasures{..}) =
    Just $
    do let bin = "pipeline-lib/measuretracts/measureTracts.py"
           tracts = WmqlTracts{..}
           showalgo =
             "WmqlTracts-(" ++
             (intercalate
                ","
                [show fstype
                ,show fs2dwitype
                ,show dwitype
                ,show dwimasktype
                ,show ukftype]) ++
             ")"
       need tracts
       vtks <-
         getDirectoryFiles ""
                           [(pathDir tracts) </> "*.vtk"]
       command_ [] bin $
         ["-f","-c","caseid","algo","-v",caseid,showalgo,"-o",(path n),"-i"] ++
         vtks

rules = rule (buildNode :: TractMeasures -> Maybe (Action [Double]))
