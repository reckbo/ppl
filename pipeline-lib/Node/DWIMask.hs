{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
module Node.DWIMask
  ( DwiMaskType (..)
  , DwiMask (..)
  , rules
  ) where

import Util (convertDwi, convertImage)
import           Data.Maybe       (fromMaybe)
import           Node.DWI         hiding (rules)
import           Node.Util
import           Paths            (outdir)
import           Shake.BuildNode
import           System.Directory as IO (renameFile)

type CaseId = String

data DwiMaskType = DwiMaskGiven
                 | DwiMaskHcp
        deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

newtype DwiMask = DwiMask (DwiMaskType, DwiType, CaseId)
        deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

instance BuildNode DwiMask where
  path (DwiMask (DwiMaskGiven, _, caseid)) = getPath "dwimask" caseid
  path n@(DwiMask (DwiMaskHcp, _, caseid))
    = outdir </> caseid </> showKey n <.> "nrrd"

  build (DwiMask (DwiMaskGiven, _, _)) = Nothing

  build n@(DwiMask (DwiMaskHcp, dwitype, caseid))
    = Just $ withTempDir $ \tmpdir -> do
    let tmpNii = tmpdir </> "dwi.nii.gz"
        dwiNode = Dwi (dwitype, caseid)
    liftIO $ convertDwi (path dwiNode) tmpNii
    unit $ command [Cwd tmpdir] "bet" [tmpNii
                            , caseid
                            , "-m"
                            , "-f"
                            , "0.1"]
    liftIO $ Util.convertImage (tmpdir </> caseid ++ "_mask.nii.gz") (path n)


rules = rule (buildNode :: DwiMask -> Maybe (Action [Double]))
