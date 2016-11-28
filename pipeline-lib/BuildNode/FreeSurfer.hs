{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE FlexibleInstances #-}
module BuildNode.FreeSurfer
  (FreeSurferType (..)
  ,rules
  ) where

import           BuildNode.MABS  (Mask (..))
import qualified FreeSurfer      as FS (runWithMask)
import qualified Paths      (t1, freeSurfer)
import           Shake.BuildNode
import BuildNode.StructuralMask (StructuralMaskType (..))
import BuildNode.Structural (StructuralType (..))

type CaseId = String

data FreeSurferType = FreeSurfer
        deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)


instance BuildNode (FreeSurferType, StructuralMaskType, CaseId) where
  path (FreeSurfer, _, caseid) = Paths.freeSurfer caseid
                             </> "mri" </> "wmparc.mgz"

  build (FreeSurfer, strctMaskType, caseid) = Just $ do
    need (T1w, caseid)
    need (strctMaskType, T1w, caseid)
    FS.runWithMask
      [5,3,0]
      (path (strctMaskType, T1w, caseid))
      (path (T1w, caseid))
      (Paths.freeSurfer caseid)

rules = rule (buildNode :: (FreeSurferType, StructuralMaskType, CaseId)
                        -> Maybe (Action [Double]))