{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
module Software.UKFTractography
  ( UKFTractography (..)
  , rules
  ) where

import           Paths                     (softwareDir)
import           Shake.BuildNode


newtype UKFTractography = UKFTractography { ukfhash :: GitHash }
        deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

instance BuildNode UKFTractography where
  path (UKFTractography hash) = softwareDir </> "UKFTractography-" ++ hash

rules = rule (buildNode :: UKFTractography -> Maybe (Action [Double]))
