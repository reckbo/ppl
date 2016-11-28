{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
module Pipeline.DWIMask
  ( DwiMaskType (..)
  , rules
  ) where

import           Pipeline.DWI   (DwiType (..))
import           Paths
import           Shake.BuildNode
import           Util            (keyToString3)

type CaseId = String

data DwiMaskType = DwiMaskSource
        deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

instance BuildNode (DwiMaskType, DwiType, CaseId) where
  path (DwiMaskSource, _, caseid) = Paths.dwimask caseid
  -- path key@(_, _, caseid) = outdir </> caseid </> keyToString3 key <.> "nrrd"

rules = rule (buildNode :: (DwiType, CaseId) -> Maybe (Action [Double]))
