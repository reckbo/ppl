{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}
module T2w
  (T2w(..)
  ,rules)
  where

import           Types
import           NodeUtil       (getPath, outdir, showKey)
import           Shake.BuildNode


data T2w =
  T2w {t2type :: T2wType
      ,caseid :: CaseId}
  deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

instance BuildNode T2w where
  path n@(T2w{..}) = case t2type of
    T2wGiven -> getPath "t2" caseid
    _ -> outdir </> caseid </> showKey n <.> "nrrd"
  build out@(T2w{..}) =
    case t2type of
      T2wGiven -> Nothing
      T2wXc ->
        Just $
        do let t2 = T2w T2wGiven caseid
           need t2
           command_ [] "pnlscripts/alignAndCenter.py" ["-i", path t2, "-o", path out]

rules = rule (buildNode :: T2w -> Maybe (Action [Double]))
