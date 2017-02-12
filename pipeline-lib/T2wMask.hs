{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}
module T2wMask where

import qualified Development.Shake    as Shake
import           Paths                (outdir)
import qualified System.Directory as IO (copyFile)
import           Shake.BuildNode
import           Software.BrainsTools
import           T1w
import           T2w
import           {-#SOURCE#-}T1wMask
import Types
import           NodeUtil

data T2wMask =
  T2wMask {t2masktype :: T2wMaskType
          ,t2type     :: T2wType
          ,caseid     :: CaseId}
  deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)


instance BuildNode T2wMask where
  path n@(T2wMask t2masktype t2type caseid) =
    case (t2masktype,t2type) of
      (NormalMask StructuralMaskGiven,T2wGiven) -> getPath "t1mask" caseid
      (NormalMask StructuralMaskGiven,T2wXc) -> getPath "t1xcmask" caseid
      _ -> outdir </> caseid </> showKey n <.> "nrrd"
  build out@(T2wMask t2masktype t2type caseid) =
    case t2masktype of
         (NormalMask StructuralMaskGiven) -> Nothing
         (NormalMask (StructuralMaskMabs bthash)) ->
           Just $
           do need T2w {..}
              withTempDir $ \tmpdir -> do
                let bt = pathDir BrainsTools {..}
                command_ [AddPath [bt] [], AddEnv "ANTSPATH" bt]
                  "pnlscripts/atlas.py" [
                  "--mabs"
                  ,"-t"
                  ,path T2w {..}
                  ,"-o"
                  ,tmpdir
                  ,"csv"
                  ,"_config/trainingDataT2Masks-hdr.csv"]
                liftIO $ IO.copyFile (tmpdir </> "mask.nrrd") (path out)
         (RigidMask bthash maskmethod t1type) ->
           Just $
           do need T2w {..}
              need T1w {..}
              let t1mask = T1wMask (NormalMask maskmethod) t1type caseid
                  bt = pathDir BrainsTools {..}
              need t1mask
              command_ [AddEnv "ANTSPATH" bt]
                       "pnlscripts/makeRigidMask.py"
                       ["-i"
                       ,path T1w {..}
                       ,"-l"
                       ,path t1mask
                       ,"-t"
                       ,path T2w {..}
                       ,"-o"
                       ,path out]


rules = rule (buildNode :: T2wMask -> Maybe (Action [Double]))
