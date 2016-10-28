{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module HCP.Topup1
  ( rules
  , TopupOutput (..)
  ) where


import           Development.Shake
import           Development.Shake.FilePath
import qualified FSL
import qualified HCP.Config                 as Paths
import           HCP.Preprocessing          (AcqParams (..), B0s (..))
import           HCP.Types                  (CaseId, PhaseOrientation (..))
import           Shake.BuildKey


--------------------------------------------------------------------------------
-- TopupConfig

data TopupConfig = TopupConfig
        deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

instance BuildKey TopupConfig where
  path _ = Paths.topupConfig_path

--------------------------------------------------------------------------------
-- TopupOutput

newtype TopupOutput = TopupOutput CaseId
        deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

instance BuildKey TopupOutput where
  paths (TopupOutput caseid) = [Paths.topupOutputPrefix_path caseid ++ "_fieldcoef.nii.gz"
                               ,Paths.topupOutputPrefix_path caseid ++ "_movpar.txt"]
  build out@(TopupOutput caseid) = Just $ do
    let acqparams = AcqParams caseid
        posb0s = B0s Pos caseid
        negb0s = B0s Neg caseid
    apply1 acqparams :: Action [Double]
    apply1 $ TopupConfig :: Action [Double]
    apply [negb0s, posb0s] :: Action [[Double]]
    withTempFile $ \posnegb0s -> do
      FSL.mergeVols posnegb0s $ map path [posb0s, negb0s]
      command [] "topup" ["--imain="++posnegb0s
                         ,"--datain="++(path $ AcqParams caseid)
                         ,"--config="++(path TopupConfig)
                         ,"--out="++(Paths.topupOutputPrefix_path caseid)
                         ,"-v"]

--------------------------------------------------------------------------------
-- Rules

rules = do
  rule (buildKey :: TopupConfig -> Maybe (Action [Double]))
  rule (buildKey :: TopupOutput -> Maybe (Action [Double]))
