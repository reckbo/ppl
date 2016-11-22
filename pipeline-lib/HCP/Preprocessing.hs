{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module HCP.Preprocessing
  (
    Dwi (..)
  , AcqParams (..)
  , Index (..)
  , Series (..)
  , B0s (..)
  , rules
  )
  where

import           Development.Shake.Config
import           FSL
import           HCP.B0sPair
import qualified HCP.Normalize            as Normalize
import           HCP.Types
import           HCP.Util                 (posOrientation, readoutTime)
import qualified PathsOutputHCP           as Paths
import           Shake.BuildNode
import           Text.Printf

--------------------------------------------------------------------------------
-- PosNegDwi

data Dwi = Dwi (Maybe PhaseOrientation) CaseId
        deriving (Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

instance FslDwi Dwi where
  nifti (Dwi Nothing caseid) = Paths.posNegDwi_path caseid
  nifti (Dwi (Just Pos) caseid) = Paths.posDwi_path caseid
  nifti (Dwi (Just Neg) caseid) = Paths.negDwi_path caseid

instance Show Dwi where
  show n@(Dwi Nothing caseid) = concat ["PosNegDwi ", caseid, " (", nifti n, ")"]
  show n@(Dwi (Just orientation) caseid)
    = concat ["Dwi ", show orientation, " ", caseid, " (", nifti n, ")"]

instance BuildNode Dwi where
  paths x = [nifti x, bval x, bvec x]

  build out@(Dwi maybeOrient caseid) = Just $
    case maybeOrient of
      Nothing -> do
        let fsldwis = [Dwi (Just Pos) caseid, Dwi (Just Neg) caseid]
        apply fsldwis :: Action [[Double]]
        makeDwi fsldwis
        trimVol (nifti out)
      (Just orientation) -> do
        fsldwis <- Normalize.getNormalizedDwis orientation caseid
        makeDwi fsldwis
    where
      makeDwi dwis = do
        vectors <- fmap concat $ traverse readBVecs dwis
        bvalues <- fmap concat $ traverse readBVals dwis
        writebvec (bvec out) vectors
        writebval (bval out) bvalues
        mergeVols (nifti out) (map nifti dwis)


--------------------------------------------------------------------------------
-- AcqParams

newtype AcqParams = AcqParams CaseId
        deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

instance BuildNode AcqParams where
  paths (AcqParams caseid) = [Paths.acqParams_path caseid]
  build n@(AcqParams caseid) = Just $ do
    -- let out = Cfg.acqParams_path caseid
    Just phaseEncoding <- fmap read <$> getConfig "phaseEncoding"
    Just echoSpacing <- fmap read <$> getConfig "echoSpacing"
    dwi0 <- head <$> Normalize.getSourceDwis Pos caseid
    b0spairs <- Normalize.getB0sPairs caseid
    phaseLength <- case posOrientation phaseEncoding of
                     PA -> read . fromStdout
                            <$> command [] "fslval" [nifti dwi0, "dim1"]
                     RL -> read . fromStdout
                            <$> command [] "fslval" [nifti dwi0, "dim2"]
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

instance BuildNode Index where
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

instance BuildNode Series where
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
-- B0s

data B0s = B0s PhaseOrientation CaseId
        deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

instance BuildNode B0s where
  paths (B0s orientation caseid) = [Paths.b0s_path orientation caseid]
  build (B0s orientation caseid) = Just $ do
    b0spairs <- Normalize.getB0sPairs caseid
    dwis <- Normalize.getNormalizedDwis orientation caseid
    let b0indices = map (_b0indicesToUse . posneg) b0spairs
              where posneg = case orientation of
                      Pos -> _pos
                      _   -> _neg
        out = Paths.b0s_path orientation caseid
    combineB0s out $ zip (map nifti dwis) b0indices

combineB0s :: FilePath -> [(FilePath, [Int])] -> Action ()
combineB0s out path_and_indices =
  do fs <- traverse (uncurry extractVols) path_and_indices
     mergeVols out fs
     trimVol out


--------------------------------------------------------------------------------
-- Rules

rules = do
  rule (buildNode :: Dwi -> Maybe (Action [Double]))
  rule (buildNode :: AcqParams -> Maybe (Action [Double]))
  rule (buildNode :: Index -> Maybe (Action [Double]))
  rule (buildNode :: Series -> Maybe (Action [Double]))
  rule (buildNode :: B0s -> Maybe (Action [Double]))
