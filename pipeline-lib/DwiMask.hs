{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}
module DwiMask
  ( DwiMask (..)
  , rules
  ) where

import           Data.Maybe      (fromMaybe)
import           Dwi             hiding (rules)
import           Paths           (outdir)
import           Shake.BuildNode
import           Types
import           Util            (convertImage)
import           NodeUtil

data DwiMask =
  DwiMask {dwimaskmethod :: DwiMaskMethod
          ,dwitype       :: DwiType
          ,caseid        :: CaseId}
  deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

instance BuildNode DwiMask where
  path (DwiMask DwiMaskGiven _ caseid) = getPath "dwimask" caseid
  path n@(DwiMask{..}) = outdir </> caseid </> showKey n <.> "nrrd"
  build out@(DwiMask{..}) =
    case dwimaskmethod of
      DwiMaskGiven -> Nothing
      DwiMaskHcp ->
        Just $
        withTempDir $
        \tmpdir ->
          do let tmpnii = tmpdir </> "dwi.nii.gz"
             need Dwi {..}
             command_ []
                      "pnlscripts/convertdwi.py"
                      ["-i", path Dwi {..}, "-o", tmpnii]
             unit $ command [] "bet" [tmpnii, tmpdir </> "dwi","-m","-f","0.1"]
             convertImage (tmpdir </> "dwi_mask.nii.gz")
                                 (path out)

rules = rule (buildNode :: DwiMask -> Maybe (Action [Double]))
