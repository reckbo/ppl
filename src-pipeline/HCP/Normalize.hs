{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module HCP.Normalize
  ( DwiPairsYaml (..)
  , MeanB0 (..)
  , HcpDwi (..)
  ,rules
  )
  where

import           Data.List
import           Data.List.Split            (splitOn)
import           Data.Yaml                  (encodeFile)
import           Development.Shake
import           Development.Shake.FilePath
import           FSL                        (BValue, FslDwi (..), extractVols_,
                                             readbval, takeBaseName', tobval,
                                             tobvec)
import           HCP.Config
import           HCP.DWIPair                (DWIInfo (..), DWIPair (..),
                                             readDWIPair)
import           HCP.Types
import           HCP.Util
import           Shake.BuildKey
import qualified System.Directory           as IO
import           Text.Printf

----------------------------------------------------------------------
-- Helper functions

posDwis = [HcpDwi SourceDwi Pos idx | idx <- [1..HCP.Config.numDwiPairs]]
negDwis = [HcpDwi SourceDwi Neg idx | idx <- [1..HCP.Config.numDwiPairs]]

getPosDwis caseid = map ($ caseid) posDwis
getNegDwis caseid = map ($ caseid) negDwis


--------------------------------------------------------------------------------
-- MeanB0

newtype MeanB0 = MeanB0 CaseId
        deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData)

instance BuildKey MeanB0 where
  paths (MeanB0 caseid) = [HCP.Config.meanB0_path caseid]
  build (MeanB0 caseid) = Just $ do
        let posdwi0 = head . getPosDwis $ caseid
        apply1 posdwi0 :: Action [Double]
        mean0 <- getB0sMean (nifti posdwi0) (bval posdwi0)
        writeFile' (HCP.Config.meanB0_path caseid) $ show mean0

getB0sMean :: FilePath -> FilePath -> Action Float
getB0sMean dwi bval = do
  b0indices <- findIndices (< b0maxbval) <$> readbval bval
  withTempFile $ \b0s -> do
    extractVols_ b0s dwi b0indices
    command_ [] "fslmaths" [b0s, "-Tmean", b0s]
    Stdout mean <- command [] "fslmeants" ["-i", b0s]
    return $ read mean


--------------------------------------------------------------------------------
-- DwiPairsYaml

newtype DwiPairsYaml = DwiPairsYaml CaseId
        deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData)

instance BuildKey DwiPairsYaml where
  paths (DwiPairsYaml caseid) = [HCP.Config.dwiPairsYaml_path caseid]
  build n@(DwiPairsYaml caseid) = Just $ do
    let posdwis = getPosDwis caseid
        negdwis = getNegDwis caseid
        out = HCP.Config.dwiPairsYaml_path caseid
    dwipairs <- traverse readDWIPair $
                zip3 [1..] (map nifti posdwis) (map nifti negdwis)
    let updatePath dwiinfo@DWIInfo{_pid=pid,_dirType=dirType}
          = dwiinfo {_dwi=dwinew}
          where dwinew = nifti $ HcpDwi NormalizedDwi dirType pid caseid
        posNew = map (updatePath._pos) dwipairs
        negNew = map (updatePath._neg) dwipairs
    liftIO $ encodeFile out $ zipWith DWIPair posNew negNew


--------------------------------------------------------------------------------
-- HcpDwi

data HcpDwi = HcpDwi DwiType Direction Int CaseId
        deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

instance FslDwi HcpDwi where
  nifti (HcpDwi dwitype direction num caseid) = case dwitype of
      SourceDwi -> HCP.Config.sourceDwi_path phasedir num caseid
      NormalizedDwi -> HCP.Config.normalizedDwi_path phasedir num caseid
    where
      phasedir = case direction of
         Pos -> posPhase phaseDirection
         Neg -> negPhase phaseDirection

instance BuildKey HcpDwi where
  paths dwi = [nifti dwi, bval dwi, bvec dwi]
  build dwiOut@(HcpDwi dwitype direction num caseid) = case dwitype of
    SourceDwi -> Nothing
    NormalizedDwi -> Just $ do
      let dwiSrc = HcpDwi SourceDwi direction num caseid
      apply1 dwiSrc :: Action [Double]
      apply1 (MeanB0 caseid) :: Action [Double]
      mean0 <- read <$> readFile' (HCP.Config.meanB0_path caseid)
      scaleDWI (nifti dwiOut) (nifti dwiSrc) (bval dwiSrc) mean0
      copyFile' (bval dwiSrc) (bval dwiOut)
      copyFile' (bvec dwiSrc) (bvec dwiOut)

scaleDWI :: FilePath -> FilePath -> FilePath -> Float -> Action ()
scaleDWI out src srcBval mean0 = do
  mean <- getB0sMean src srcBval
  command_ [] "fslmaths" [src
                         ,"-mul", show mean0
                         ,"-div", show mean
                         ,out]


--------------------------------------------------------------------------------
-- Rules

rules :: Rules ()
rules = do
    rule (buildKey :: DwiPairsYaml -> Maybe (Action [Double]))
    rule (buildKey :: MeanB0 -> Maybe (Action [Double]))
    rule (buildKey :: HcpDwi -> Maybe (Action [Double]))
