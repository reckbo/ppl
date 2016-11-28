{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
module Pipeline.UKFTractography
  ( UKFTractographyExe (..)
  , UKFTractographyType (..)
  , rules
  ) where

import           Pipeline.DWI     (DwiType (..))
import           Pipeline.DWIMask (DwiMaskType (..))
import           Control.Monad     (unless, when)
import qualified Paths
import           Shake.BuildNode
import qualified System.Directory  as IO
import           Util              (buildGitHubCMake, keyToString4)


newtype UKFTractographyExe = UKFTractographyExe GitHash
        deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

instance BuildNode UKFTractographyExe where
  path (UKFTractographyExe hash) = Paths.ukfTractographyExePrefix ++ "-" ++  hash

  build out@(UKFTractographyExe hash) = Just $ do
    clonedir <- liftIO . IO.makeAbsolute $ takeDirectory (path out)
      </> "UKFTractography-" ++ hash ++ "-tmp"
    buildGitHubCMake [] "pnlbwh/ukftractography" hash clonedir
    liftIO $ IO.renameFile (clonedir
                            </> "_build"
                            </> "UKFTractography-build/ukf/bin/UKFTractography") (path out)
    liftIO $ IO.removeDirectoryRecursive clonedir

type CaseId = String

type Params = [(String, String)]

data UKFTractographyType = UKFTractographyDefault
                         | UKFTractography Params
        deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

defaultParams :: Params
defaultParams = [("Ql","70")
               ,("Qm","0.001")
               ,("Rs","0.015")
               ,("numTensor","2")
               ,("recordLength","1.7")
               ,("seedFALimit","0.18")
               ,("seedsPerVoxel","10")
               ,("stepLength","0.3")]

formatParams :: Params -> [String]
formatParams ps = concatMap (\(arg,val) -> ["--"++arg,val]) ps

instance BuildNode (UKFTractographyType, DwiType, DwiMaskType, CaseId) where
  path key@(UKFTractographyDefault, _, _, caseid)
    = Paths.ukfTractographyDir caseid </> keyToString4 key <.> "vtk"

  path key@(UKFTractography params, _, _, caseid)
    = Paths.ukfTractographyDir caseid
      </> params2dirs params
      </> "UKFTractography-" ++ caseid
      <.> "vtk"
      where params2dirs = foldr (</>) "" . map snd

  build key@(ukftype, dwitype, dwimasktype, caseid) = Just $ do
    Just exeNode <- fmap UKFTractographyExe <$> getConfig "UKFTractography-hash"
    need exeNode
    need (dwitype, caseid)
    need (dwimasktype, dwitype, caseid)
    let params = case ukftype of
          UKFTractographyDefault -> defaultParams
          (UKFTractography params) -> params
    cmd (path exeNode) (["--dwiFile", path (dwitype, caseid)
                        ,"--maskFile", path (dwimasktype, dwitype, caseid)
                        ,"--seedsFile", path (dwimasktype, dwitype, caseid)
                        ,"--recordTensors"
                        ,"--tracts", path key] ++ formatParams params)

rules :: Rules ()
rules = do
  rule (buildNode :: UKFTractographyExe -> Maybe (Action [Double]))
  rule (buildNode :: (UKFTractographyType, DwiType, DwiMaskType, CaseId) -> Maybe (Action [Double]))
