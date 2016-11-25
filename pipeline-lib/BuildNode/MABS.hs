{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module BuildNode.MABS
  ( Mask (..)
  , rules
  ) where

import qualified ANTs                     (applyTransforms, computeWarp)
import qualified BuildNode.ANTs
import           Control.Monad            (unless)
import           Data.List                (intercalate)
import           Data.List.Split          (splitOn)
import           Development.Shake.Config
import qualified FSL                      (average, threshold)
import qualified Paths               (t1, t1MaskMabs)
import           Shake.BuildNode
import qualified System.Directory         as IO (copyFile)
import           Util                     (convertImage)
import           System.IO.Temp   (withSystemTempFile)
import qualified Development.Shake as Shake (need)

type CaseId = String

data Mask = Mask CaseId
             deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

instance BuildNode Mask where
  path (Mask caseid) = Paths.t1MaskMabs caseid

  build out@(Mask caseid) = Just $ withTempDir $ \tmpdir -> do
      Just antsNode <- fmap BuildNode.ANTs.ANTs <$> getConfig "ANTs-hash"
      let t1Target = Paths.t1 caseid
      apply1 antsNode :: Action [Double]
      -- TODO sanitize user input csv, or change to use config
      trainingPairs <- map (splitOn ",")
                       <$> readFileLines "config/trainingDataT1.csv"
      Shake.need . concat $ trainingPairs
      registeredmasks <- liftIO $ traverse
        (\(trainingVol:trainingMask:_) -> register (path antsNode)
          tmpdir (trainingVol, trainingMask) (Paths.t1 caseid))
        trainingPairs
      let tmpnii = tmpdir </>  "tmp.nii.gz"
      FSL.average tmpnii registeredmasks
      FSL.threshold 0.5 tmpnii tmpnii
      liftIO $ Util.convertImage tmpnii (path out)

rules = rule (buildNode :: Mask -> Maybe (Action [Double]))

register :: FilePath -> FilePath -> (FilePath, FilePath) -> FilePath -> IO FilePath
register antsPath outdir (moving, movingMask) fixed
  = let outMask = outdir
                  </> (intercalate "-"
                        ["Mask",takeBaseName movingMask,"in",takeBaseName moving])
                  ++ ".nii.gz"
    in withSystemTempFile ".nii.gz" $ \tmpwarp _ -> do
      liftIO $ ANTs.computeWarp antsPath moving fixed tmpwarp
      liftIO $ ANTs.applyTransforms antsPath "" [tmpwarp] movingMask fixed outMask
      return outMask
