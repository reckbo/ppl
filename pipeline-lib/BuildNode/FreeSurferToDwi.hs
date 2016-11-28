{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE ExistentialQuantification    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
module BuildNode.FreeSurferToDwi
  (
  ) where

import           BuildNode.ANTs
import           BuildNode.DWI
import           BuildNode.DWIMask
import           BuildNode.FreeSurfer
import           BuildNode.Structural
import           BuildNode.StructuralMask
import qualified Paths
import           PipelineRegistrations    (freesurferToDwiWithMasks, makeRigidMask)
import           Shake.BuildNode
import           Util                     (keyToString5)
import BuildNode.FreeSurfer (FreeSurferType (..))
import FreeSurfer
import Data.Foldable (traverse_)
-- import qualified System.Directory as IO (copyFile)

type CaseId = String

data FsInDwiType = FsInDwi
             deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

instance BuildNode (FsInDwiType, StructuralMaskType, DwiType, DwiMaskType, CaseId) where
  path key@(_, _, _, _, caseid) = Paths.fsInDwiDir caseid </> keyToString5 key

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



-- rules = rule (buildNode :: (Fs, CaseId) -> Maybe (Action [Double]))
