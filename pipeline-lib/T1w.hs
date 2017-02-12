{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}
module T1w
  (T1w(..)
  ,rules)
  where

import           Types
import           NodeUtil       (getPath, outdir, showKey)
import           Shake.BuildNode
import           Util


data T1w =
  T1w {t1type :: T1wType
      ,caseid :: CaseId}
  deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

instance BuildNode T1w where
  path n@(T1w{..}) =
    case t1type of
      T1wGiven -> getPath "t1" caseid
      _ -> outdir </> caseid </> showKey n <.> "nrrd"
  build out@(T1w{..}) =
    case t1type of
      T1wGiven -> Nothing
      T1wXc ->
        Just $
        do let t1 = T1w T1wGiven caseid
           need t1
           command_ [] "pnlscripts/alignAndCenter.py" ["-i", path t1, "-o", path out]

rules = rule (buildNode :: T1w -> Maybe (Action [Double]))
