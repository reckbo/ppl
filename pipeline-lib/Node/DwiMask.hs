{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
module Node.DwiMask
  ( DwiMask (..)
  , rules
  ) where

import           Data.Maybe       (fromMaybe)
import           Node.Dwi         hiding (rules)
import           Node.Types
import           Node.Util
import           Paths            (outdir)
import           Shake.BuildNode
import           System.Directory as IO (renameFile)
import           Util             (convertDwi, convertImage)

data DwiMask =
  DwiMask {dwimaskmethod :: DwiMaskMethod
          ,dwitype :: DwiType
          ,caseid :: CaseId}
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
          do let tmpNii = tmpdir </> "dwi.nii.gz"
	     need Dwi{..}
             convertDwi (path Dwi{..})
                        tmpNii
             unit $ command [] "bet" [tmpNii,tmpdir </> "dwi","-m","-f","0.1"]
             liftIO $
               Util.convertImage (tmpdir </> "dwi_mask.nii.gz")
                                 (path out)

rules = rule (buildNode :: DwiMask -> Maybe (Action [Double]))
