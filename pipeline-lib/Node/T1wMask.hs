{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}
module Node.T1wMask where

import qualified ANTs                      (makeRigidMask)
import           Data.List                 (intercalate)
import           Data.List.Split           (splitOn)
import qualified Development.Shake         as Shake
import           MABS                      (mabs)
import           Node.Software.BrainsTools
import           Node.T2w
import           Node.T2wMask
import           Node.T1w
import           Node.Types
import           Node.Util
import           Paths                     (outdir)
import           Shake.BuildNode

data T1wMask =
  T1wMask {t1masktype :: T1wMaskType
          ,t1type     :: T1wType
          ,caseid     :: CaseId}
  deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

instance BuildNode T1wMask where
  path n@(T1wMask t1masktype t1type caseid) = case (t1masktype, t1type) of
    (NormalMask StructuralMaskGiven, T1wGiven) -> getPath "t1mask" caseid
    (NormalMask StructuralMaskGiven, T1wXc) -> getPath "t1xcmask" caseid
    _ -> outdir </> caseid </> showKey n <.> "nrrd"

  build out@(T1wMask t1masktype t1type caseid) = case t1masktype of
    (NormalMask StructuralMaskGiven) -> Nothing
    (NormalMask (StructuralMaskMabs bthash)) -> Just $ do
      need T1w{..}
      runMabs (BrainsTools{..}) "config/trainingDataT1.csv" (path T1w{..}) (path out)
    (RigidMask bthash maskmethod t2type) -> Just $ do
      need T1w{..}
      need T2w{..}
      let t2mask = T2wMask (NormalMask maskmethod) t2type caseid
      need t2mask
      ANTs.makeRigidMask (path BrainsTools{..}) (path t2mask) (path T2w{..}) (path T1w{..}) (path out)

runMabs :: BrainsTools -> FilePath -> FilePath -> FilePath -> Action ()
runMabs brainstools csv strct out =
  do trainingPairs <- map (splitOn ",") <$> readFileLines csv
     Shake.need . concat $ trainingPairs
     need brainstools
     mabs (pathDir brainstools) trainingPairs strct out


rules = rule (buildNode :: T1wMask -> Maybe (Action [Double]))
