{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}
module TractMeasures
  (TractMeasures(..)
  ,rules)
  where

import           Data.List       (intercalate)
import           Paths           (outdir)
import           Shake.BuildNode
import           Types
import           NodeUtil
import           WmqlTracts      hiding (rules)


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
    do let bin = "pnlscripts/measuretracts/measureTracts.py"
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
