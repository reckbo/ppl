{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}
module UKFTractography
  (UKFTractography (..)
  , rules
  ) where

import           Dwi                      hiding (rules)
import           DwiMask                  hiding (rules)
import qualified Software.UKFTractography as Soft
import           Types
import           NodeUtil                     (getPath, showKey)
import qualified Paths
import           Shake.BuildNode
import           Util                          (convertImage)

data UKFTractography =
  UKFTractography {ukftype       :: UKFTractographyType
                  ,ukfhash       :: String
                  ,dwimaskpair :: (DwiType, DwiMaskMethod)
                  ,caseid        :: CaseId}
  deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

instance BuildNode UKFTractography where
  path n@(UKFTractography{..}) = case ukftype of
    UKFTractographyGiven -> getPath "ukf" caseid
    _ -> Paths.outdir </> caseid </> showKey n <.> "vtk"
  build out@(UKFTractography ukftype ukfhash (dwitype,dwimaskmethod) caseid) = case ukftype of
    UKFTractographyGiven -> Nothing
    _ -> Just $
      do let bin = Soft.UKFTractography {..}
             params =
               case ukftype of
                 UKFTractographyDefault       -> defaultParams
                 UKFTractographyCustom params -> params
         need bin
         need Dwi {..}
         need DwiMask {..}
         withTempDir $
           \tmpdir ->
             do let dwinrrd = tmpdir </> "dwi.nrrd"
                    dwimasknrrd = tmpdir </> "dwimask.nrrd"
                command_ [] "pnlscripts/convertdwi.py" ["-i", path Dwi{..}, "-o", dwinrrd]
                convertImage (path DwiMask{..}) dwimasknrrd
                command_ []
                         (path bin)
                         (["--dwiFile"
                          ,dwinrrd
                          ,"--maskFile"
                          ,dwimasknrrd
                          ,"--seedsFile"
                          ,dwimasknrrd
                          ,"--recordTensors"
                          ,"--tracts"
                          ,path out] ++
                          formatParams params)

defaultParams :: Params
defaultParams = [("Ql","70")
               ,("Qm","0.001")
               ,("Rs","0.015")
               ,("numTensor","2")
               ,("recordLength","1.7")
               ,("seedFALimit","0.18")
               ,("seedsPerVoxel","10")
               ,("stepLength","0.3")]

formatParams :: Params -> [String]
formatParams ps = concatMap (\(arg,val) -> ["--"++arg,val]) ps


rules :: Rules ()
rules = rule (buildNode :: UKFTractography -> Maybe (Action [Double]))