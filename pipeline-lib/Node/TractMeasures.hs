{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}
module Node.TractMeasures
  (TractMeasures(..)
  ,rules)
  where

import           Data.List       (intercalate)
import           Node.Types
import           Node.Util
import           Node.WmqlTracts hiding (rules)
import           Paths           (outdir)
import           Shake.BuildNode


data TractMeasures =
  TractMeasures {tqhash        :: GitHash
                ,bthash        :: GitHash
                ,fstype        :: FreeSurferType
                ,fs2dwimethod  :: FsToDwiMethod
                ,dwitype       :: DwiType
                ,dwimaskmethod :: DwiMaskMethod
                ,ukfhash       :: GitHash
                ,ukftype       :: UKFTractographyType
                ,caseid        :: CaseId}
  deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)


instance BuildNode TractMeasures where
  path n@(TractMeasures{..}) = outdir </> caseid </> showKey n <.> "csv"
  build n@(TractMeasures{..}) =
    Just $
    do let bin = "pipeline-lib/measuretracts/measureTracts.py"
           showalgo = showKey WmqlTracts {..}
       need WmqlTracts {..}
       vtks <-
         getDirectoryFiles ""
                           [(pathDir WmqlTracts {..}) </> "*.vtk"]
       command_ [] bin $
         ["-f"
         ,"-c"
         ,"caseid"
         ,"algo"
         ,"-v"
         ,caseid
         ,showKey WmqlTracts {..}
         ,"-o"
         ,(path n)
         ,"-i"] ++
         vtks

rules = rule (buildNode :: TractMeasures -> Maybe (Action [Double]))
