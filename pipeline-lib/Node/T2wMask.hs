{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}
module Node.T2wMask where

import qualified ANTs                      (makeRigidMask)
import           Data.List                 (intercalate)
import           Data.List.Split           (splitOn)
import qualified Development.Shake         as Shake
import           MABS                      (mabs)
import           Node.Software.BrainsTools
import           Node.T1w
import           {-# SOURCE #-} Node.T1wMask
import           Node.T2w
import           Node.Types
import           Node.Util
import           Paths                     (outdir)
import           Shake.BuildNode

data T2wMask =
  T2wMask {t2masktype :: T2wMaskType
          ,t2type     :: T2wType
          ,caseid     :: CaseId}
  deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

instance BuildNode T2wMask where
  path n@(T2wMask t2masktype t2type caseid) = case (t2masktype, t2type) of
    (NormalMask StructuralMaskGiven, T2wGiven) -> getPath "t2mask" caseid
    (NormalMask StructuralMaskGiven, T2wXc) -> getPath "t2xcmask" caseid
    _ -> outdir </> caseid </> showKey n <.> "nrrd"

  build out@(T2wMask t2masktype t2type caseid) = case t2masktype of
    (NormalMask StructuralMaskGiven) -> Nothing
    (NormalMask (StructuralMaskMabs bthash)) -> Just $ do
      need T2w{..}
      runMabs (BrainsTools{..}) "config/trainingDataT2.csv" (path T2w{..}) (path out)
    (RigidMask bthash maskmethod t1type) -> Just $ do
      need T2w{..}
      need T1w{..}
      let t1mask = T1wMask (NormalMask maskmethod) t1type caseid
      need t1mask
      ANTs.makeRigidMask (path BrainsTools{..}) (path t1mask) (path T1w{..}) (path T2w{..}) (path out)

rules = rule (buildNode :: T2wMask -> Maybe (Action [Double]))
