{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Software.BrainsTools
  (BrainsTools (..)
  ,rules
  ) where

import           NodeUtil        (showKey)
import           Paths           (softwareDir)
import           Shake.BuildNode


newtype BrainsTools = BrainsTools { bthash :: GitHash }
        deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

instance BuildNode BrainsTools where
  paths (BrainsTools hash) =
    map (combine base)
        ["antsRegistrationSyN.sh"
        ,"ANTS"
        ,"antsRegistration"
        ,"antsApplyTransforms"
        ,"ComposeMultiTransform"
        ,
         -- ,"PrintHeader"
         "ResampleImageBySpacing"]
    where base = softwareDir </> "BRAINSTools-bin-" ++ hash

rules = rule (buildNode :: BrainsTools -> Maybe (Action [Double]))
