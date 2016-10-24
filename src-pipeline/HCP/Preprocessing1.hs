{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module HCP.Preprocessing1
  (
    PosNegDwi (..)
  , rules
  )
  where

import           FSL
import qualified HCP.Config     as Cfg
import qualified HCP.Normalize  as Normalize
import           HCP.Types
import           Shake.BuildKey


newtype PosNegDwi = PosNegDwi CaseId
        deriving (Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

instance FslDwi PosNegDwi where
  nifti (PosNegDwi caseid) = Cfg.posNegVol_path caseid

instance Show PosNegDwi where
  show n@(PosNegDwi caseid) = concat ["PosNegDwi ", caseid, " (", nifti n, ")"]

instance BuildKey PosNegDwi where
  paths x = [nifti x, bval x, bvec x]
  build out@(PosNegDwi caseid) = Just $ do
    posDwis <- Normalize.getSourcePosDwis caseid
    negDwis <- Normalize.getSourceNegDwis caseid
    apply $ posDwis ++ negDwis :: Action [[Double]]
    posvectors <- fmap concat $ traverse readBVecs posDwis
    negvectors <- fmap concat $ traverse readBVecs negDwis
    posbvals <- fmap concat $ traverse readBVals posDwis
    negbvals <- fmap concat $ traverse readBVals negDwis
    writebvec (bvec out) $ posvectors ++ negvectors
    writebval (bval out) $ posbvals ++ negbvals
    mergeVols (nifti out) (map nifti $ posDwis ++ negDwis)
    trimVol (nifti out)

rules = do
  rule (buildKey :: PosNegDwi -> Maybe (Action [Double]))
  Normalize.rules

-- newtype AcqParams = AcqParams CaseId
--         deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

-- instance BuildKey AcqParams where
--   paths (AcqParams caseid) = HCP.Config.acqParams_path caseid
--   build (AcqParams caseid) = Just $ do
--     let out = Cfg.acqParams_path caseid
--         dwipairs <- getYaml
--         let dwi0 = _dwi._pos.head $ dwipairs
--         need [dwi0]
--         phaselength <- case phasedir of
--                          PA -> read . fromStdout <$> command [] "fslval" [dwi0, "dim1"]
--                          _ -> read . fromStdout <$> command [] "fslval" [dwi0, "dim2"]
--         let readout = printf "%.6f" $ readoutTime phaselength echospacing
--             numB0sToUse = length . concatMap _b0indicesToUse
--             acqParamsPos =  case phasedir of
--               PA -> "0 1 0 " ++ readout
--               RL -> "1 0 0 " ++ readout
--             acqParamsNeg = case phasedir of
--               PA -> "0 -1 0 " ++ readout
--               RL -> "-1 0 0 " ++ readout
--             acq = replicate (numB0sToUse $ map _pos dwipairs) acqParamsPos
--             acq' = replicate (numB0sToUse $ map _neg dwipairs) acqParamsNeg
--         writeFile' out $ unlines (acq ++ acq')
