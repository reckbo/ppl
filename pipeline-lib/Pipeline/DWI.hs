{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
module Pipeline.DWI
  ( DwiType (..)
  , rules
  ) where

import qualified Pipeline.HCP
import           Paths
import           Shake.BuildNode
import           Util            (keyToString)

type CaseId = String

data DwiType = DwiSource
             | DwiXc
             | DwiHcp
        deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

instance BuildNode (DwiType, CaseId) where
  path (DwiSource, caseid) = Paths.dwi caseid
  path key@(_, caseid) = outdir </> caseid </> keyToString key <.> ".nrrd"

  build (DwiSource, _) = Nothing

  build key@(DwiXc, caseid) = Just $ do
    need (DwiSource, caseid)
    command_ [] "config/axis_align_nrrd.py" ["-i", path (DwiSource, caseid)
                                            ,"-o", path key]
    command_ [] "config/center.py" ["-i", path key
                                   ,"-o", path key]

  build key@(DwiHcp, caseid) = Just $ do
    need $ Pipeline.HCP.HcpDwi caseid
    return ()

rules = rule (buildNode :: (DwiType, CaseId) -> Maybe (Action [Double]))
