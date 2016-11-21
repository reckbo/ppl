{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module HCP.Topup
  ( rules
  , TopupOutput (..)
  , HiFiB0 (..)
  , NoDifBrainMask (..)
  ) where


import           Development.Shake
import           Development.Shake.FilePath
import qualified FSL
import           HCP.Preprocessing          (AcqParams (..), B0s (..))
import           HCP.Types                  (CaseId, PhaseOrientation (..))
import qualified HcpOutputPaths                  as Paths
import           Shake.BuildNode
import           Text.Printf

--------------------------------------------------------------------------------
-- NoDifBrainMask

newtype NoDifBrainMask = NoDifBrainMask CaseId
        deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

instance BuildNode NoDifBrainMask where
  path (NoDifBrainMask caseid) = Paths.noDifBrainMaskPrefix_path caseid ++
    "_mask.nii.gz"

  pathPrefix (NoDifBrainMask caseid) = Paths.noDifBrainMaskPrefix_path caseid

  build out@(NoDifBrainMask caseid) = Just $ do
    apply1 (HiFiB0 caseid) :: Action [Double]
    command [] "bet" [(path $ HiFiB0 caseid)
                     , pathPrefix out
                     , "-m"
                     , "-f"
                     , "0.2"]


--------------------------------------------------------------------------------
-- TopupConfig

data TopupConfig = TopupConfig
        deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

instance BuildNode TopupConfig where
  -- path _ = InputPaths.topupConfig_path
  path _ = "config/hcp_b02b0.cnf"

--------------------------------------------------------------------------------
-- HiFiB0

newtype HiFiB0 = HiFiB0 CaseId
        deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

instance BuildNode HiFiB0 where
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
                                 ,"--topup=" ++ (pathPrefix $ TopupOutput caseid)
                                 ,"--datain=" ++ (path $ AcqParams caseid)
                                 ,"--inindex=1," ++ show dimt
                                 ,"--out=" ++ (path out)
                                 ]


--------------------------------------------------------------------------------
-- TopupOutput

newtype TopupOutput = TopupOutput CaseId
        deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

instance BuildNode TopupOutput where
  paths (TopupOutput caseid) = [Paths.topupOutputPrefix_path caseid ++ "_fieldcoef.nii.gz"
                               ,Paths.topupOutputPrefix_path caseid ++ "_movpar.txt"]
  pathPrefix (TopupOutput caseid) = Paths.topupOutputPrefix_path caseid

  build out@(TopupOutput caseid) = Just $ do
    let acqparams = AcqParams caseid
        posb0s = B0s Pos caseid
        negb0s = B0s Neg caseid
    apply1 acqparams :: Action [Double]
    apply1 $ TopupConfig :: Action [Double]
    apply [negb0s, posb0s] :: Action [[Double]]
    withTempFile $ \posnegb0s -> do
      FSL.mergeVols posnegb0s $ map path [posb0s, negb0s]
      command [] "topup" ["--imain=" ++ posnegb0s
                         ,"--datain=" ++ (path $ AcqParams caseid)
                         ,"--config=" ++ path TopupConfig
                         ,"--out=" ++ pathPrefix out
                         ,"-v"]

--------------------------------------------------------------------------------
-- Rules

rules = do
  rule (buildNode :: TopupConfig -> Maybe (Action [Double]))
  rule (buildNode :: TopupOutput -> Maybe (Action [Double]))
  rule (buildNode :: HiFiB0 -> Maybe (Action [Double]))
  rule (buildNode :: NoDifBrainMask -> Maybe (Action [Double]))
