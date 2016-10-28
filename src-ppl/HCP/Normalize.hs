{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module HCP.Normalize
  ( B0sPairsYaml (..)
  , MeanB0 (..)
  , DwiScan (..)
  , getSourceDwis
  , getNormalizedDwis
  , getB0sPairs
  , rules
  )
  where

import           Data.List
import           Data.List.Split            (splitOn)
import           Data.Yaml                  (encodeFile)
import           Development.Shake
import           Development.Shake.Config
import           Development.Shake.FilePath
import           FSL                        (BValue (..), FslDwi (..), extractVols_,
                                             readbval, takeBaseName', tobval,
                                             tobvec)
import           qualified HCPConfig  as Paths
import           HCP.B0sPair                (B0sPair (..), mkB0sPair)
import           HCP.Types
import           HCP.Util
import           Shake.BuildKey
import qualified System.Directory           as IO
import           Text.Printf
import Data.Yaml

----------------------------------------------------------------------
-- Helper functions

--------------------------------------------------------------------------------
-- MeanB0

newtype MeanB0 = MeanB0 CaseId
        deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData)

instance BuildKey MeanB0 where
  paths (MeanB0 caseid) = [Paths.meanB0_path caseid]
  build (MeanB0 caseid) = Just $ do
        posdwi0 <- head <$> getSourceDwis Pos caseid
        mean0 <- getB0sMean (nifti posdwi0) (bval posdwi0)
        writeFile' (Paths.meanB0_path caseid) $ show mean0

getB0sMean :: FilePath -> FilePath -> Action Float
getB0sMean dwi bval = do
  Just b0maxbval <- fmap (BValue . read) <$> getConfig "b0MaxBVal"
  b0indices <- findIndices (< b0maxbval) <$> readbval bval
  withTempFile $ \b0s -> do
    extractVols_ b0s dwi b0indices
    command_ [] "fslmaths" [b0s, "-Tmean", b0s]
    Stdout mean <- command [] "fslmeants" ["-i", b0s]
    return $ read mean


--------------------------------------------------------------------------------
-- B0sPairsYaml

newtype B0sPairsYaml = B0sPairsYaml CaseId
        deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData)

instance BuildKey B0sPairsYaml where
  paths (B0sPairsYaml caseid) = [Paths.b0sPairsYaml_path caseid]
  build n@(B0sPairsYaml caseid) = Just $ do
    let  out = Paths.b0sPairsYaml_path caseid
    posdwis <- getSourceDwis Pos caseid
    negdwis <- getSourceDwis Neg caseid
    apply $ posdwis ++ negdwis :: Action [[Double]]
    Just b0MaxBVal <- fmap (BValue . read) <$> getConfig "b0MaxBVal"
    Just b0Dist <- fmap read <$> getConfig "b0Dist"
    posbvals <- traverse readBVals posdwis
    negbvals <- traverse readBVals negdwis
    liftIO $ encodeFile out $ zipWith (mkB0sPair b0MaxBVal b0Dist) posbvals negbvals

getB0sPairs :: CaseId -> Action [B0sPair]
getB0sPairs caseid = do
    apply1 $ B0sPairsYaml caseid :: Action [Double]
    Just b0spairs <- liftIO . decodeFile . Paths.b0sPairsYaml_path $ caseid
    return b0spairs


--------------------------------------------------------------------------------
-- DwiScan

data DwiScan = NormalizedDwiScan PhaseOrientation Int CaseId
             | SourceDwiScan PhaseOrientation Int CaseId
        deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

instance FslDwi DwiScan where
  nifti (NormalizedDwiScan orientation num caseid) =
      Paths.normalizedDwi_path orientation num caseid
  nifti (SourceDwiScan orientation num caseid) =
      Paths.sourceDwi_path orientation num caseid

instance BuildKey DwiScan where
  paths dwi = [nifti dwi, bval dwi, bvec dwi]
  build (SourceDwiScan _ _ _) = Nothing
  build outdwi@(NormalizedDwiScan orientation num caseid) = Just $ do
      let srcdwi = SourceDwiScan orientation num caseid
      apply1 srcdwi :: Action [Double]
      apply1 (MeanB0 caseid) :: Action [Double]
      mean0 <- read <$> readFile' (Paths.meanB0_path caseid)
      scaleDWI (nifti outdwi) (nifti srcdwi) (bval srcdwi) mean0
      copyFile' (bval srcdwi) (bval outdwi)
      copyFile' (bvec srcdwi) (bvec outdwi)

scaleDWI :: FilePath -> FilePath -> FilePath -> Float -> Action ()
scaleDWI out src srcBval mean0 = do
  mean <- getB0sMean src srcBval
  command_ [] "fslmaths" [src
                         ,"-mul", show mean0
                         ,"-div", show mean
                         ,out]

getSourceDwis :: PhaseOrientation -> CaseId -> Action [DwiScan]
getSourceDwis orientation caseid = do
  Just numPairs <- fmap read <$> getConfig "numDwiPairs"
  let dwis = [SourceDwiScan orientation idx caseid | idx <- [1..numPairs]]
  apply dwis :: Action [[Double]]
  return dwis

getNormalizedDwis :: PhaseOrientation -> CaseId -> Action [DwiScan]
getNormalizedDwis orientation caseid = do
  Just numPairs <- fmap read <$> getConfig "numDwiPairs"
  let dwis = [NormalizedDwiScan orientation idx caseid | idx <- [1..numPairs]]
  apply dwis :: Action [[Double]]
  return dwis


--------------------------------------------------------------------------------
-- Rules

rules :: Rules ()
rules = do
    rule (buildKey :: B0sPairsYaml -> Maybe (Action [Double]))
    rule (buildKey :: MeanB0 -> Maybe (Action [Double]))
    rule (buildKey :: DwiScan -> Maybe (Action [Double]))
