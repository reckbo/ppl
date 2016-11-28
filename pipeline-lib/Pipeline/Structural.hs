{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
module Pipeline.Structural
  ( StructuralType (..)
  , rules
  ) where

import qualified Paths
import           Shake.BuildNode

type CaseId = String

data StructuralType = T1w | T2w
                    deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

instance BuildNode (StructuralType, CaseId) where
  path (T1w, caseid) = Paths.t1 caseid
  path (T2w, caseid) = Paths.t2 caseid


rules = rule (buildNode :: (StructuralType, CaseId) -> Maybe (Action [Double]))
