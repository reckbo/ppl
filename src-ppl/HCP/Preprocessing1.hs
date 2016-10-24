{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveAnyClass              #-}
module HCP.Preprocessing1
  (
    PosNegDwi (..)
  , AcqParams (..)
  , Index (..)
  , Series (..)
  , rules
  )
  where

import           FSL
import qualified HCP.Config     as Paths
import qualified HCP.Normalize  as Normalize
import           HCP.Types
import           Shake.BuildKey
import HCP.B0sPair
import HCP.Util (posOrientation, readoutTime)
import Development.Shake.Config
import Text.Printf

--------------------------------------------------------------------------------
-- PosNegDwi

newtype PosNegDwi = PosNegDwi CaseId
        deriving (Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

instance FslDwi PosNegDwi where
  nifti (PosNegDwi caseid) = Paths.posNegVol_path caseid

instance Show PosNegDwi where
  show n@(PosNegDwi caseid) = concat ["PosNegDwi ", caseid, " (", nifti n, ")"]

instance BuildKey PosNegDwi where
  paths x = [nifti x, bval x, bvec x]
  build out@(PosNegDwi caseid) = Just $ do
    posDwis <- Normalize.getNormalizedDwis Pos caseid
    negDwis <- Normalize.getNormalizedDwis Neg caseid
    posvectors <- fmap concat $ traverse readBVecs posDwis
    negvectors <- fmap concat $ traverse readBVecs negDwis
    posbvals <- fmap concat $ traverse readBVals posDwis
    negbvals <- fmap concat $ traverse readBVals negDwis
    writebvec (bvec out) $ posvectors ++ negvectors
    writebval (bval out) $ posbvals ++ negbvals
    mergeVols (nifti out) (map nifti $ posDwis ++ negDwis)
    trimVol (nifti out)


--------------------------------------------------------------------------------
-- AcqParams

newtype AcqParams = AcqParams CaseId
        deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

instance BuildKey AcqParams where
  paths (AcqParams caseid) = [Paths.acqParams_path caseid]
  build n@(AcqParams caseid) = Just $ do
    -- let out = Cfg.acqParams_path caseid
    Just phaseEncoding <- fmap read <$> getConfig "phaseEncoding"
    Just echoSpacing <- fmap read <$> getConfig "echoSpacing"
    dwi0 <- head <$> Normalize.getSourceDwis Pos caseid
    b0spairs <- Normalize.getB0sPairs caseid
    phaseLength <- case posOrientation phaseEncoding of
                     PA -> read . fromStdout <$> command [] "fslval" [nifti dwi0, "dim1"]
                     RL -> read . fromStdout <$> command [] "fslval" [nifti dwi0, "dim2"]
    let readout = printf "%.6f" $ readoutTime phaseLength echoSpacing
        numB0sToUse = length . concatMap _b0indicesToUse
        acqParamsPos =  case posOrientation phaseEncoding of
              PA -> "0 1 0 " ++ readout
              RL -> "1 0 0 " ++ readout
        acqParamsNeg = case posOrientation phaseEncoding of
              PA -> "0 -1 0 " ++ readout
              RL -> "-1 0 0 " ++ readout
        acq = replicate (numB0sToUse $ map _pos b0spairs) acqParamsPos
        acq' = replicate (numB0sToUse $ map _neg b0spairs) acqParamsNeg
    writeFile' (Paths.acqParams_path caseid) $ unlines (acq ++ acq')


--------------------------------------------------------------------------------
-- Index

newtype Index = Index CaseId
        deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

instance BuildKey Index where
  paths (Index caseid) = [Paths.index_path caseid]
  build (Index caseid) = Just $ do
    b0spairs <- Normalize.getB0sPairs caseid
    writeFile' (Paths.index_path caseid) (unlines $ map show $ mkIndexList b0spairs)

mkIndexList :: [B0sPair] -> [Int]
mkIndexList b0spairs = mkIndex' $ addLast b0indices size
  where
    posSizes = map (_size . _pos) b0spairs
    negSizes = map (_size . _neg) b0spairs
    sizes = scanl (+) 0 $ posSizes ++ negSizes
    size = head . reverse $ sizes
    posb0indices = map (_b0indicesToUse . _pos) b0spairs
    negb0indices = map (_b0indicesToUse . _neg) b0spairs
    b0indices = concat $ zipWith (\is sz -> map (+sz) is) (posb0indices++negb0indices) sizes
    mkIndex' is = reverse $ foldl g [] is
      where g res i =
              let dx = i - length res
                  val = case res of
                    [] -> 1
                    _ -> 1 + head res
              in (replicate dx val) ++ res

addLast :: [a] -> a -> [a]
addLast xs y = reverse . (y:) . reverse $ xs

--------------------------------------------------------------------------------
-- Series

data Series = Series PhaseOrientation CaseId
        deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

instance BuildKey Series where
  paths (Series orientation caseid) = [Paths.series_path orientation caseid]
  build (Series orientation caseid) = Just $ do
    ps <- Normalize.getB0sPairs caseid
    let
      minsizes = zipWith min (map (_size._pos) $ ps) (map (_size._neg) $ ps)
      series = zipWith printline minsizes $ map (_size. posneg) ps
        where posneg = case orientation of
                Pos -> _pos
                Neg -> _neg
      printline x y = printf "%d %d" x y
    writeFile' (Paths.series_path orientation caseid) $ unlines series



--------------------------------------------------------------------------------
-- Rules

rules = do
  rule (buildKey :: PosNegDwi -> Maybe (Action [Double]))
  rule (buildKey :: AcqParams -> Maybe (Action [Double]))
  rule (buildKey :: Index -> Maybe (Action [Double]))
  rule (buildKey :: Series -> Maybe (Action [Double]))
  Normalize.rules
