{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module HCP.Topup1
  ( rules
  , TopupOutput (..)
  ) where


import           Development.Shake
import           Development.Shake.FilePath
import qualified FSL
import           HCP.Preprocessing          (AcqParams (..), B0s (..))
import           HCP.Types                  (CaseId, PhaseOrientation (..))
import qualified HCPConfig                  as Paths
import           Shake.BuildKey
import           Text.Printf


--------------------------------------------------------------------------------
-- TopupConfig

data TopupConfig = TopupConfig
        deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

instance BuildKey TopupConfig where
  path _ = Paths.topupConfig_path

--------------------------------------------------------------------------------
-- HiFiB0

newtype HiFiB0 = HiFiB0 CaseId
        deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

instance BuildKey HiFiB0 where
  path (HiFiB0 caseid) = Paths.hiFiB0_path caseid

  build out@(HiFiB0 caseid) = Just $ do
    let (posb0s, negb0s) = (B0s Pos caseid, B0s Neg caseid)
    apply [posb0s, negb0s] :: Action [[Double]]
    apply1 (AcqParams caseid) :: Action [Double]
    dimt <- (+1) <$> FSL.getDim4 (path posb0s)
    withTempFile $ \posb01 ->
      withTempFile $ \negb01 -> do
        FSL.extractVol_ posb01 (path posb0s) 1
        FSL.extractVol_ negb01 (path negb0s) 1
        command_ [] "applytopup" [printf "--imain=%s,%s" posb01 negb01
                                 ,"--topup="++Paths.applyTopupOutputPrefix_path caseid
                                 ,"--datain=" ++ (path $ AcqParams caseid)
                                 ,"--inindex=1,"++ show dimt
                                 ,"--out="++ (path out)
                                 ]


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
