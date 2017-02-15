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

data T1w =
  T1w {t1type :: T1wType
      ,caseid :: CaseId}
  deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

instance BuildNode T1w where
  path n@(T1w{..}) =
    case t1type of
      T1wGiven key -> getPath key caseid
      _ -> outdir </> caseid </> showKey n <.> "nrrd"
  build out@(T1w{..}) =
    case t1type of
      T1wGiven _ -> Nothing
      T1wXc key ->
        Just $
        do let t1 = T1w (T1wGiven key) caseid
           need t1
           command_ [] "pnlscripts/alignAndCenter.py" ["-i", path t1, "-o", path out]

rules = rule (buildNode :: T1w -> Maybe (Action [Double]))
