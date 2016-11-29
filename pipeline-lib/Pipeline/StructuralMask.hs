{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
module Pipeline.StructuralMask
  ( StructuralMaskType (..)
  , StructuralType (..)
  , rules
  ) where

import qualified ANTs
import           Data.List             (intercalate)
import           Data.List.Split       (splitOn)
import           Data.Maybe            (fromMaybe)
import qualified Development.Shake     as Shake
import qualified FSL                   (average, threshold)
import           MABS                  (mabs)
import qualified Paths
import           Pipeline.ANTs         (ANTs (..))
import           Pipeline.Structural   (StructuralType (..))
import           PipelineRegistrations (makeRigidMask)
import           Shake.BuildNode
import qualified System.Directory      as IO (copyFile)
import           System.IO.Temp        (withSystemTempFile)
import           Util                  (convertImage)

type CaseId = String

data StructuralMaskType = StructuralMaskMabs
                        | StructuralMaskSource
                        | StructuralMaskRigidMabs
                        | StructuralMaskRigidSource
                        deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

instance BuildNode (StructuralMaskType, StructuralType, CaseId) where
  path (StructuralMaskSource, T1w, caseid) = fromMaybe
    (error "Set 't1mask' in Paths.hs") $ Paths.t1mask caseid
  path (StructuralMaskSource, T2w, caseid) = fromMaybe
    (error "Set 't2mask' in Paths.hs") $ Paths.t2mask caseid
  path (StructuralMaskMabs, T1w, caseid) = Paths.t1mabs caseid
  path (StructuralMaskMabs, T2w, caseid) = Paths.t2mabs caseid
  path (StructuralMaskRigidMabs, T1w, caseid) = Paths.t1rigidmabs caseid
  path (StructuralMaskRigidMabs, T2w, caseid) = Paths.t2rigidmabs caseid
  path (StructuralMaskRigidSource, T1w, caseid) = Paths.t1rigidmask caseid
  path (StructuralMaskRigidSource, T2w, caseid) = Paths.t2rigidmask caseid

  build key@(StructuralMaskSource, structtype, _) = Nothing

  build key@(StructuralMaskMabs, strctType, caseid) =  Just $ do
      antsPath <- getAnts
      need (strctType, caseid)
      -- TODO sanitize user input csv, or change to use config
      trainingPairs <- map (splitOn ",")
                       <$> readFileLines "config/trainingDataT1.csv"
      Shake.need . concat $ trainingPairs
      mabs antsPath
        trainingPairs (path (strctType, caseid)) (path key)

  build key@(StructuralMaskRigidSource, strctType, caseid)
    = Just $ makeRigidFrom StructuralMaskSource (switch strctType) caseid (path key)

  build key@(StructuralMaskRigidMabs, strctType, caseid)
    = Just $ makeRigidFrom StructuralMaskMabs (switch strctType) caseid (path key)

makeRigidFrom StructuralMaskRigidMabs _ _ _ = error ""
makeRigidFrom StructuralMaskRigidSource _ _ _ = error ""
makeRigidFrom srcMaskType srcStrctType caseid out = do
    let strctType = switch srcStrctType
    antsPath <- getAnts
    need (srcMaskType, srcStrctType, caseid)
    need (srcStrctType, caseid)
    need (strctType, caseid)
    liftIO $ makeRigidMask antsPath
      (path (srcMaskType, srcStrctType, caseid))
      (path (srcStrctType, caseid))
      (path (strctType, caseid))
      out

switch T1w = T2w
switch T2w = T1w

getAnts = do
    Just antsNode <- fmap ANTs <$> getConfig "ANTs-hash"
    need antsNode
    return . takeDirectory . path $ antsNode

rules = rule (buildNode :: (StructuralMaskType, StructuralType, CaseId) -> Maybe (Action [Double]))
