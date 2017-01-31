{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}
module Node.StructuralMask
  (StructuralMask (..)
  , rules
  ) where

import           ANTs                      (makeRigidMask)
import           Data.List.Split           (splitOn)
import qualified Development.Shake         as Shake
import           MABS                      (mabs)
import           Node.Software.BrainsTools hiding (rules)
import           Node.Structural           (Structural (..))
import           Node.Types
import           Node.Util
import           Paths                     (outdir)
import           Shake.BuildNode

data StructuralMask =
  StructuralMask {strctmasktype :: StructuralMaskType
                 ,strcttype     :: StructuralType
                 ,caseid        :: CaseId}
  deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

instance BuildNode StructuralMask where
  path (StructuralMask StructuralMaskSource strct caseid)
    | strct == T1w = getPath "t1mask" caseid
    | strct == T2w = getPath "t2mask" caseid
  path n@(StructuralMask{..}) = outdir </> caseid </> showKey n <.> "nrrd"
  build out@(StructuralMask{..}) =
    case strctmasktype of
      StructuralMaskSource -> Nothing
      StructuralMaskMabs bthash ->
        Just $
        do need Structural {..}
           let csv =
                 case strcttype of
                   T1w -> "config/trainingDataT1.csv"
                   T2w -> "config/trainingDataT2.csv"
		   StructuralXC T1w -> "config/trainingDataT1.csv"
		   StructuralXC T2w -> "config/trainingDataT2.csv"
           trainingPairs <- map (splitOn ",") <$> readFileLines csv
           Shake.need . concat $ trainingPairs
           mabs (pathDir BrainsTools {..})
                trainingPairs
                (path $ Structural {..})
                (path out)
      StructuralMaskRigid bthash movingmasktype ->
        case movingmasktype of
          (StructuralMaskRigid _ _) -> error "StructuralMask: recursive"
          _ ->
            Just $
            do let movingstrcttype =
                     case strcttype of
                       T1w -> T2w
                       T2w -> T1w
               let movingmask =
                     StructuralMask movingmasktype movingstrcttype caseid
                   movingstrct = Structural movingstrcttype caseid
               need movingmask
               need movingstrct
               need Structural {..}
               makeRigidMask (pathDir BrainsTools {..})
                             (path movingmask)
                             (path movingstrct)
                             (path Structural {..})
                             (path out)


rules = rule (buildNode :: StructuralMask -> Maybe (Action [Double]))
