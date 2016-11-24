{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module BuildNode.Freesurfer
  (
    ) where

import           Shake.BuildNode
import BuildNode.MABS (Mask (..))
import qualified Freesurfer as FS (runWithMask)
import qualified PathsOutput (freeSurfer)
import qualified PathsInput (t1)

type CaseId = String

newtype FreeSurfer = FreeSurfer CaseId
        deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)


instance BuildNode FreeSurfer where
  path (FreeSurfer caseid) = PathsOutput.freeSurfer caseid
                             </> "mri" </> "wmparc.mgz"

  build (FreeSurfer caseid) = Just $ do
    let maskNode = BuildNode.MABS.Mask caseid
    apply1 maskNode :: Action [Double]
    liftIO $ FS.runWithMask [5,3,0] (path maskNode) (PathsInput.t1 caseid)
      (PathsOutput.freeSurfer caseid)