{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
module Node.StructuralMask
  (StructuralMask (..)
  , rules
  ) where

import           ANTs              (makeRigidMask)
import           Data.List         (intercalate)
import           Data.List.Split   (splitOn)
import           Data.Maybe        (fromMaybe)
import qualified Development.Shake as Shake
import qualified FSL               (average, threshold)
import           MABS              (mabs)
import           Node.ANTs         (ANTs (..), getAntsPath)
import           Node.Structural   (Structural (..))
import           Node.Types
import           Node.Util
import           Paths             (outdir)
import           Shake.BuildNode
import qualified System.Directory  as IO (copyFile)
import           System.IO.Temp    (withSystemTempFile)
import           Util              (convertImage)

newtype StructuralMask
  = StructuralMask (StructuralMaskType, StructuralType, CaseId)
  deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

instance BuildNode StructuralMask where
  path (StructuralMask (StructuralMaskSource, T1w, caseid)) =
        getPath "t1mask" caseid
  path (StructuralMask (StructuralMaskSource, T2w, caseid)) =
        getPath "t2mask" caseid
  path n@(StructuralMask (_, _, caseid))
      = outdir </> caseid </> showKey n <.> "nrrd"

  build (StructuralMask (StructuralMaskSource, _, _)) = Nothing

  -- TODO only works for T1w
  build node@(StructuralMask (StructuralMaskMabs, strctType, caseid)) =  Just $ do
      antsPath <- getAntsPath
      need $ Structural (strctType, caseid)
      -- TODO sanitize user input csv, or change to use config
      trainingPairs <- map (splitOn ",")
                       <$> readFileLines "config/trainingDataT1.csv"
      Shake.need . concat $ trainingPairs
      mabs antsPath
        trainingPairs (path $ Structural (strctType, caseid)) (path node)

  build (StructuralMask ((StructuralMaskRigid (StructuralMaskRigid _)) , _, _))
    = error "StructuralMask: Recursive"

  build node@(StructuralMask (StructuralMaskRigid srcMaskType, strctType, caseid))
    = Just $ do
    let srcStrctType = case strctType of
          T1w -> T2w
          T2w -> T1w
    antsPath <- getAntsPath
    let mask = StructuralMask (srcMaskType, srcStrctType, caseid)
        strct = Structural (srcStrctType, caseid)
        target = Structural (strctType, caseid)
    need mask
    need strct
    need target
    liftIO $ makeRigidMask antsPath (path mask) (path strct) (path target) (path node)

rules = rule (buildNode :: StructuralMask -> Maybe (Action [Double]))
