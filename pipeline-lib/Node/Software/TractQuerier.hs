{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Node.Software.TractQuerier
  (TractQuerier (..)
  ,rules
  ) where

import           Node.Util (showKey)
import           Paths                     (softwareDir)
import           Shake.BuildNode


newtype TractQuerier = TractQuerier { tqhash :: GitHash }
        deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

instance BuildNode TractQuerier where
  paths (TractQuerier hash) =
    map (combine base)
        ["README.md", "scripts/tract_querier","scripts/tract_math"]
    where base = softwareDir </> "tract_querier" ++ hash


rules = rule (buildNode :: TractQuerier -> Maybe (Action [Double]))
