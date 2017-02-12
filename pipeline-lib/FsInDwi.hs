{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}
module FsInDwi
  (FsInDwi (..)
  ,rules
  ) where

import           Dwi                  hiding (rules)
import           DwiMask              hiding (rules)
import           FreeSurfer           hiding (rules)
import           Software.BrainsTools hiding (rules)
import           T1w                  hiding (rules)
import           T1wMask              hiding (rules)
import           T2w                  hiding (rules)
import           Types
import           NodeUtil                 (showKey)
import qualified Paths
import           Shake.BuildNode
import           System.Directory          as IO (copyFile)

data FsInDwi =
  FsInDwi {bthash        :: GitHash
          ,fs2dwimethod  :: FsToDwiMethod
          ,fstype        :: FreeSurferType
          ,dwitype       :: DwiType
          ,dwimaskmethod :: DwiMaskMethod
          ,caseid        :: CaseId}
  deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

instance BuildNode FsInDwi where
  path n@(FsInDwi{..}) = Paths.outdir </> caseid </> showKey n <.> "nii.gz"
  build out@(FsInDwi{..}) =
    case fs2dwimethod of
      (FsBrain_T1_T2_B0 t1type t2type t1masktype) -> undefined
        -- Just $
        -- withTempDir $
        -- \tmpdir ->
        --   do let fsInDwiDir = (dropExtensions $ path out)
        --          t2mask = tmpdir </> "t2mask.nrrd"
        --      need FreeSurfer {..}
        --      need Dwi {..}
        --      need DwiMask {..}
        --      need T1wMask {..}
        --      need T1w {..}
        --      need T2w {..}
        --      ANTs.makeRigidMask (pathDir BrainsTools{..}) (path T1wMask{..}) (path T1w{..}) (path T2w{..}) t2mask
        --      ANTs.freesurferToDwiWithMasks (pathDir BrainsTools {..})
        --                (pathDir FreeSurfer{..})
        --                (path Dwi{..})
        --                (path DwiMask{..})
        --                (path T1w{..})
        --                (path T1wMask{..})
        --                t2mask
        --                (path T2w{..})
        --                fsInDwiDir
        --      liftIO $ IO.renameFile (fsInDwiDir </> "wmparc-in-dwi.nii.gz")
        --                    (path out)
      FsBrain_B0 -> Just $ withTempDir $ \tmpdir -> do
        need FreeSurfer {..}
        need Dwi {..}
        need DwiMask {..}
        let bt = pathDir BrainsTools{..}
            fs = takeDirectory . pathDir $ FreeSurfer{..}
        command_ [AddPath [bt] [], AddEnv "ANTSPATH" bt] "pnlscripts/fs2dwi.py"
          ["-f", fs, "-t", path Dwi{..}, "-m", path DwiMask{..}
          ,"-o", tmpdir]
        liftIO $ IO.copyFile (tmpdir </> "wmparcInDwi1mm.nii.gz") (path out)


rules = rule (buildNode :: FsInDwi -> Maybe (Action [Double]))
