{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Pipeline.HCP.Normalize
  ( B0sPairsYaml (..)
  , MeanB0 (..)
  , Dwi (..)
  , getB0sPairs
  , rules
  )
  where

import Data.Maybe (fromMaybe)
import           Data.List (findIndices)
import           Data.List.Split      (splitOn)
import           Data.Maybe           (fromJust)
import           Data.Yaml            (decodeFile, encodeFile)
import           FSL                  (BValue (..), FslDwi (..), extractVols_,
                                       mergeVols, readbval, takeBaseName',
                                       tobval, tobvec, writebval, writebvec)
import           Pipeline.HCP.B0sPair (B0sPair (..), mkB0sPair)
import           Pipeline.HCP.Types
import           Pipeline.HCP.Util
import           Pipeline.Util        (showKey)
import           Shake.BuildNode
import qualified System.Directory     as IO
import qualified Paths                (dwiHcp, hcpdir)

stage = "0_Normalize"

newtype MeanB0 = MeanB0 CaseId
        deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData)

instance BuildNode MeanB0 where
  path (MeanB0 caseid) =  Paths.hcpdir caseid stage </> "meanB0.txt"
  build k@(MeanB0 caseid) = Just $ do
    let pos0 = (DwiSource Pos 0, caseid)
    need pos0
    mean0 <- getB0sMean (nifti pos0) (bval pos0)
    writeFile' (path k) $ show mean0

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

newtype B0sPairsYaml = B0sPairsYaml [Int]
        deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData)

instance BuildNode (B0sPairsYaml, CaseId) where
  path k@(B0sPairsYaml _, caseid) = Paths.hcpdir caseid stage </> showKey k
  build k@(B0sPairsYaml indices, caseid) = Just $ do
    let posdwis = [(DwiSource Pos idx, caseid) | idx <- indices]
        negdwis = [(DwiSource Neg idx, caseid) | idx <- indices]
    needs $ posdwis ++ negdwis
    Just b0MaxBVal <- fmap (BValue . read) <$> getConfig "b0MaxBVal"
    Just b0Dist <- fmap read <$> getConfig "b0Dist"
    posbvals <- traverse readBVals posdwis
    negbvals <- traverse readBVals negdwis
    liftIO $ encodeFile (path k) $
      zipWith (mkB0sPair b0MaxBVal b0Dist) posbvals negbvals

getB0sPairs :: CaseId -> [Int] -> Action [B0sPair]
getB0sPairs caseid indices = do
    let k = (B0sPairsYaml indices, caseid)
    need k
    liftIO $ fmap fromJust . decodeFile . path $ k


--------------------------------------------------------------------------------
-- Dwi

data Dwi = DwiSource PhaseOrientation Int
         | DwiNormalized PhaseOrientation Int
         | DwiJoined PhaseOrientation [Int]
         | DwiJoinedAll [Int]
        deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

instance FslDwi (Dwi, CaseId) where
  nifti (DwiSource orientation num, caseid)
    = fromMaybe (error "Set 'dwiHcp path in Paths.hs") $ 
      Paths.dwiHcp orientation num caseid
  nifti k@(_, caseid) = Paths.hcpdir caseid stage </> showKey k <.> "nii.gz"

instance BuildNode (Dwi, CaseId) where
  paths dwi = [nifti dwi, bval dwi, bvec dwi]

  build (DwiSource _ _, _) = Nothing

  build k@(DwiNormalized orientation idx, caseid) = Just $ do
    let src = (DwiSource orientation idx, caseid)
    need src
    need (MeanB0 caseid)
    mean0 <- liftIO $ fmap read $ readFile . path $ (MeanB0 caseid)
    scaleDWI (nifti src) (nifti src) (bval src) mean0
    copyFile' (bval src) (bval k)
    copyFile' (bvec src) (bvec k)

  build k@(DwiJoined orient indices, caseid) = Just $ do
    let scans = [(DwiNormalized orient idx, caseid) | idx <- indices]
    needs scans
    joinDwis k scans

  build k@(DwiJoinedAll indices, caseid) = Just $ do
    let scans = [(DwiNormalized orient idx, caseid)
                | idx <- indices
                , orient <- [Pos, Neg]]
    needs scans
    joinDwis k scans

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
    rule (buildNode :: (B0sPairsYaml, CaseId) -> Maybe (Action [Double]))
    rule (buildNode :: MeanB0 -> Maybe (Action [Double]))
    rule (buildNode :: (Dwi, CaseId) -> Maybe (Action [Double]))
