{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleInstances         #-}
module Pipeline.FsInDwi
  ( rules
  , FsInDwiType (..)
  ) where

import           Pipeline.ANTs           hiding (rules)
import           Pipeline.DWI            hiding (rules)
import           Pipeline.DWIMask        hiding (rules)
import           Pipeline.FreeSurfer     hiding (rules)
import           Pipeline.Structural     hiding (rules)
import           Pipeline.StructuralMask hiding (rules)
import           Data.Foldable            (traverse_)
import qualified Paths
import           PipelineRegistrations    (freesurferToDwiWithMasks,
                                           makeRigidMask)
import           Shake.BuildNode
import           Pipeline.Util                     (showKey)

type CaseId = String

data FsInDwiType = FsInDwi
             deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

instance BuildNode (FsInDwiType, StructuralMaskType, DwiType, DwiMaskType, CaseId) where
  path key@(_, _, _, _, caseid) = Paths.fsInDwiDir caseid </> showKey key

  build key@(FsInDwi, strctmaskType, dwiType, dwimaskType, caseid)
    = Just $ withTempDir $ \tmpdir -> do
      Just antsNode <- fmap ANTs <$> getConfig "ANTs-hash"
      need antsNode
      let fsdirN = (FreeSurfer, strctmaskType, caseid)
          dwiN = (dwiType, caseid)
          dwiMaskN = (dwimaskType, dwiType, caseid)
          t2N = (T2w, caseid)
          t1N = (T1w, caseid)
          t1MaskN = (strctmaskType, T1w, caseid)
      need fsdirN
      need dwiN
      need dwiMaskN
      need t2N
      need t1N
      need t1MaskN
      let t2mask = tmpdir </> "t2mask.nii.gz"
      liftIO $ makeRigidMask (path antsNode)
        (path t1MaskN)
        (path t1N)
        (path t2N)
        t2mask
      liftIO $ freesurferToDwiWithMasks (path antsNode)
        (path fsdirN)
        (path dwiN) (path dwiMaskN)
        (path t1N) (path t1MaskN)
        (path t2N) t2mask
        (path key)


rules = rule (buildNode :: (FsInDwiType, StructuralMaskType, DwiType, DwiMaskType, CaseId) -> Maybe (Action [Double]))
