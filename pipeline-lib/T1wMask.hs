{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}
module T1wMask where

import qualified Development.Shake    as Shake
import           Paths                (outdir)
import qualified System.Directory as IO (copyFile)
import           Shake.BuildNode
import           Software.BrainsTools
import           T1w
import           T2w
import           T2wMask
import           Types
import           NodeUtil

data T1wMask =
  T1wMask {t1masktype :: T1wMaskType
          ,t1type     :: T1wType
          ,caseid     :: CaseId}
  deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)


instance BuildNode T1wMask where
  path n@(T1wMask t1masktype t1type caseid) =
    case t1masktype of
      (NormalMask (StructuralMaskGiven key)) -> getPath key caseid
      _ -> outdir </> caseid </> showKey n <.> "nrrd"
  build out@(T1wMask t1masktype t1type caseid) =
    case t1masktype of
         (NormalMask (StructuralMaskGiven _)) -> Nothing
         (NormalMask (StructuralMaskMabs bthash)) ->
           let bt = pathDir BrainsTools {..} in
           Just $
           do need T1w {..}
              withTempDir $ \tmpdir -> do
                command_ [AddPath [bt] [], AddEnv "ANTSPATH" bt]
                  "pnlscripts/atlas.py" [
                    "--mabs"
                    ,"-t"
                    ,path T1w {..}
                    ,"-o"
                    ,tmpdir
                    ,"csv"
                    ,"_config/trainingDataT1AHCC-hdr.csv"]
                liftIO $ IO.copyFile (tmpdir </> "mask.nrrd") (path out)
         (RigidMask bthash maskmethod t2type) ->
           let bt = pathDir BrainsTools {..} in
           Just $
           do need T1w {..}
              need T2w {..}
              let t2mask = T2wMask (NormalMask maskmethod) t2type caseid
              need t2mask
              command_ [AddEnv "ANTSPATH" bt]
                       "pnlscripts/makeRigidMask.py"
                       ["-i"
                       ,path T2w {..}
                       ,"-l"
                       ,path t2mask
                       ,"-t"
                       ,path T1w {..}
                       ,"-o"
                       ,path out]


rules = rule (buildNode :: T1wMask -> Maybe (Action [Double]))
