{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
module Pipeline.FreeSurfer
  ( rules
  , FreeSurferType (..)
  ) where

import           Pipeline.Structural     hiding (rules)
import           Pipeline.StructuralMask hiding (rules)
import qualified FreeSurfer               (runWithMask)
import qualified Paths                    (freeSurfer, t1)
import           Shake.BuildNode

type CaseId = String

data FreeSurferType = FreeSurfer
        deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)


instance BuildNode (FreeSurferType, StructuralMaskType, CaseId) where
  path (FreeSurfer, _, caseid) = Paths.freeSurfer caseid
                             </> "mri" </> "wmparc.mgz"

  build (FreeSurfer, strctMaskType, caseid) = Just $ do
    need (T1w, caseid)
    need (strctMaskType, T1w, caseid)
    FreeSurfer.runWithMask
      [5,3,0]
      (path (strctMaskType, T1w, caseid))
      (path (T1w, caseid))
      (Paths.freeSurfer caseid)

rules = rule (buildNode :: (FreeSurferType, StructuralMaskType, CaseId)
                        -> Maybe (Action [Double]))
