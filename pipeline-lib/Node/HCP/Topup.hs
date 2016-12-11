{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
module Node.HCP.Topup
  ( rules
  , TopupOutput (..)
  , HiFiB0 (..)
  , Mask (..)
  ) where


import qualified FSL
import           Node.HCP.Preprocessing (AcqParams (..), B0s (..))
import           Node.HCP.Types         (CaseId, PhaseOrientation (..))
import           Node.HCP.Util          (hcppath)
import           Node.Util              (showKey)
import           Shake.BuildNode
import           System.Directory       as IO
import           Text.Printf

stage = "2_Topup"

data Mask = Mask [Int] CaseId
        deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

instance BuildNode Mask where
  path k@(Mask _ caseid) = hcppath caseid stage k <.> "nii.gz"

  build k@(Mask indices caseid) = Just $ withTempDir $ \tmpdir -> do
    need (HiFiB0 indices caseid)
    unit $ command [] "bet" [(path $ HiFiB0 indices caseid)
                            , tmpdir </> "pre"
                            , "-m"
                            , "-f"
                            , "0.2"]
    unit $ cmd "mv" (tmpdir </> "pre_mask.nii.gz") (path k)



data TopupConfig = TopupConfig deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

instance BuildNode TopupConfig where
  path _ = "config/hcp_b02b0.cnf"


data HiFiB0 = HiFiB0 [Int] CaseId
        deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

instance BuildNode HiFiB0 where
  path k@(HiFiB0 _ caseid) = hcppath caseid stage k <.> "nii.gz"

  build k@(HiFiB0 indices caseid) = Just $ do
    let (posb0s, negb0s) = (B0s Pos indices caseid, B0s Neg indices caseid)
    needs [posb0s, negb0s]
    need (AcqParams indices caseid)
    dimt <- (+1) <$> FSL.getDim4 (path posb0s)
    withTempFile $ \posb01 ->
      withTempFile $ \negb01 -> do
        FSL.extractVol_ posb01 (path posb0s) 1
        FSL.extractVol_ negb01 (path negb0s) 1
        command_ [] "applytopup" [printf "--imain=%s,%s" posb01 negb01
                                 ,"--topup=" ++ (hcppath caseid stage (TopupOutput indices caseid))
                                 ,"--datain=" ++ (path $ AcqParams indices caseid)
                                 ,"--inindex=1," ++ show dimt
                                 ,"--out=" ++ (path k)
                                 ]


data TopupOutput = TopupOutput [Int] CaseId
        deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

instance BuildNode TopupOutput where
  paths n@(TopupOutput indices caseid)
    = map (\f -> hcppath caseid stage n ++ f)
      ["_fieldcoef.nii.gz", "_movpar.txt"]

  build n@(TopupOutput indices caseid) = Just $ do
    let acqparams = AcqParams indices caseid
        posb0s = B0s Pos indices caseid
        negb0s = B0s Neg indices caseid
    need acqparams
    need TopupConfig
    needs [negb0s, posb0s]
    withTempFile $ \posnegb0s -> do
      FSL.mergeVols posnegb0s $ map path [posb0s, negb0s]
      command [] "topup" ["--imain=" ++ posnegb0s
                         ,"--datain=" ++ (path $ AcqParams indices caseid)
                         ,"--config=" ++ path TopupConfig
                         ,"--out=" ++ (hcppath caseid stage n)
                         ,"-v"]

rules = do
  rule (buildNode :: TopupConfig -> Maybe (Action [Double]))
  rule (buildNode :: TopupOutput -> Maybe (Action [Double]))
  rule (buildNode :: HiFiB0 -> Maybe (Action [Double]))
  rule (buildNode :: Mask -> Maybe (Action [Double]))
