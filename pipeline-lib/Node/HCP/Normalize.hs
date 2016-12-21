{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Node.HCP.Normalize
  ( B0sPairsYaml (..)
  , MeanB0 (..)
  , DwiN (..)
  , DwiTypeN (..)
  , getB0sPairs
  , rules
  )
  where

import           Data.List        (findIndices)
import           Data.List.Split  (splitOn)
import           Data.Maybe       (fromMaybe)
import           Data.Maybe       (fromJust)
import           Data.Yaml        (decodeFile, encodeFile)
import           FSL              (BValue (..), FslDwi (..), extractVols_,
                                   mergeVols, readbval, takeBaseName', tobval,
                                   tobvec, writebval, writebvec)
import           Node.HCP.B0sPair (B0sPair (..), mkB0sPair)
import           Node.HCP.Types
import           Node.HCP.Util    (hcppath)
import           Node.Util
import           Shake.BuildNode
import qualified System.Directory as IO (copyFile)

stage = "0_Normalize"

newtype MeanB0 = MeanB0 ([Int], CaseId)
        deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData)

instance BuildNode MeanB0 where
  path n@(MeanB0 (indices, caseid)) =  hcppath caseid stage n  <.> "txt"

  build n@(MeanB0 (indices, caseid)) = Just $ do
    let pos0 = DwiN (DwiScan Pos (head indices), caseid)
    need pos0
    mean0 <- getB0sMean (nifti pos0) (bval pos0)
    liftIO $ writeFile (path n) $ show mean0

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

newtype B0sPairsYaml = B0sPairsYaml ([Int], CaseId)
        deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData)

instance BuildNode B0sPairsYaml where
  path n@(B0sPairsYaml (_, caseid)) = hcppath caseid stage n <.> "yaml"
  build n@(B0sPairsYaml (indices, caseid)) = Just $ do
    let posdwis = [DwiN (DwiScan Pos idx, caseid) | idx <- indices]
        negdwis = [DwiN (DwiScan Neg idx, caseid) | idx <- indices]
    needs $ posdwis ++ negdwis
    Just b0MaxBVal <- fmap (BValue . read) <$> getConfig "b0MaxBVal"
    Just b0Dist <- fmap read <$> getConfig "b0Dist"
    posbvals <- traverse readBVals posdwis
    negbvals <- traverse readBVals negdwis
    liftIO $ encodeFile (path n) $
      zipWith (mkB0sPair b0MaxBVal b0Dist) posbvals negbvals

getB0sPairs :: CaseId -> [Int] -> Action [B0sPair]
getB0sPairs caseid indices = do
    let n = B0sPairsYaml (indices, caseid)
    need n
    liftIO $ fmap fromJust . decodeFile . path $ n


--------------------------------------------------------------------------------
-- Dwi

data DwiTypeN = DwiScan PhaseOrientation Int
             | DwiNormalized PhaseOrientation [Int] Int
             | DwiJoined PhaseOrientation [Int]
             | DwiJoinedAll [Int]
        deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

newtype DwiN = DwiN (DwiTypeN, CaseId)
        deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

instance FslDwi DwiN where
  nifti n@(DwiN (DwiScan Pos num, caseid)) =
        rplc "{num}" (show num) (getPath "dwiHcpPos" caseid)

  nifti n@(DwiN (DwiScan Neg num, caseid)) =
        rplc "{num}" (show num) (getPath "dwiHcpNeg" caseid)

  nifti n@(DwiN (_, caseid)) = hcppath caseid stage n <.> "nii.gz"

instance BuildNode DwiN where
  paths dwi = [nifti dwi, bval dwi, bvec dwi]

  build (DwiN (DwiScan _ _, _)) = Nothing

  build n@(DwiN (DwiNormalized orientation indices idx, caseid)) = Just $ do
    let src = (DwiN (DwiScan orientation idx, caseid))
    need src
    need $ MeanB0 (indices, caseid)
    mean0 <- liftIO $ fmap read $ readFile . path $ MeanB0 (indices, caseid)
    scaleDWI (nifti n) (nifti src) (bval src) mean0
    liftIO $ IO.copyFile (bval src) (bval n)
    liftIO $ IO.copyFile (bvec src) (bvec n)

  build n@(DwiN (DwiJoined orient indices, caseid)) = Just $ do
    let scans = [DwiN (DwiNormalized orient indices idx, caseid) | idx <- indices]
    needs scans
    joinDwis n scans

  build n@(DwiN (DwiJoinedAll indices, caseid)) = Just $ do
    let scans = [DwiN (DwiNormalized orient indices idx, caseid)
                | idx <- indices
                , orient <- [Pos, Neg]]
    needs scans
    joinDwis n scans

joinDwis out dwis = do
        vectors <- fmap concat $ traverse readBVecs dwis
        bvalues <- fmap concat $ traverse readBVals dwis
        writebvec (bvec out) vectors
        writebval (bval out) bvalues
        mergeVols (nifti out) (map nifti dwis)

scaleDWI :: FilePath -> FilePath -> FilePath -> Float -> Action ()
scaleDWI out src srcBval mean0 = do
  mean <- getB0sMean src srcBval
  command_ [] "fslmaths" [src ,"-mul", show mean0 ,"-div", show mean ,out]


rules :: Rules ()
rules = do
    rule (buildNode :: B0sPairsYaml -> Maybe (Action [Double]))
    rule (buildNode :: MeanB0 -> Maybe (Action [Double]))
    rule (buildNode :: DwiN -> Maybe (Action [Double]))
