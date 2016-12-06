{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
module Node.FreeSurfer
  ( rules
  , FreeSurferType (..)
  , FreeSurfer (..)
  ) where

import           Data.Maybe
import qualified FreeSurfer          (runWithMask)
import           Node.Structural     hiding (rules)
import           Node.StructuralMask hiding (rules)
import           Node.Util
import           Paths               (outdir)
import           Shake.BuildNode

type CaseId = String

data FreeSurferType = FreeSurferGiven
                    | FreeSurferWithMask StructuralMaskType
                    deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

newtype FreeSurfer = FreeSurfer (FreeSurferType, CaseId)
                   deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

instance BuildNode FreeSurfer where
  -- Picking wmparc.mgz as representative of freesurfer subject directory
  paths (FreeSurfer (FreeSurferGiven, caseid))
    = map (\f -> getPath "freesurfer" caseid </> f) [ "mri/brain.mgz"
                                                    , "mri/wmparc.mgz"]

  paths n@(FreeSurfer (FreeSurferWithMask _, caseid)) =
    [outdir </> caseid </> showKey n </> "mri/brain.mgz"
    ,outdir </> caseid </> showKey n </> "mri/wmparc.mgz"]

  build n@(FreeSurfer (FreeSurferGiven, _)) = Nothing

  build n@(FreeSurfer (FreeSurferWithMask masktype, caseid)) = Just $ do
    let strct = Structural (T1w, caseid)
    let mask = StructuralMask (masktype, T1w, caseid)
    need mask
    need strct
    FreeSurfer.runWithMask
      [5,3,0]
      (path mask)
      (path strct)
      (pathDir n)

rules = rule (buildNode :: FreeSurfer -> Maybe (Action [Double]))
