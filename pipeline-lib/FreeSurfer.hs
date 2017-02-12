{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}
module FreeSurfer
  (FreeSurfer (..)
  , rules
  ) where

import           Shake.BuildNode
import           T1w                 hiding (rules)
import           T1wMask             hiding (rules)
import           Types
import           NodeUtil

data FreeSurfer =
  FreeSurfer {fstype :: FreeSurferType
             ,caseid :: CaseId}
  deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)


instance BuildNode FreeSurfer where
  paths n@(FreeSurfer fstype caseid) =
    case fstype of
      FreeSurferGiven ->
        map (\f -> getPath "freesurfer" caseid </> f)
            ["mri/brain.mgz","mri/wmparc.mgz"]
      _ ->
        [outdir </> caseid </> showKey n </> "mri/brain.mgz"
        ,outdir </> caseid </> showKey n </> "mri/wmparc.mgz"]
  build out@(FreeSurfer{..}) =
    case fstype of
      FreeSurferGiven -> Nothing
      (FreeSurferUsingMask t1type t1masktype) ->
        Just $
        do need T1w {..}
           need T1wMask {..}
           command_ []
                    "pnlscripts/fs.py"
                    ["-i"
                    ,path T1w {..}
                    ,"-m"
                    ,path T1wMask {..}
                    ,"-f"
                    ,"-o"
                    ,(takeDirectory . takeDirectory . path $ out)]

rules = rule (buildNode :: FreeSurfer -> Maybe (Action [Double]))
