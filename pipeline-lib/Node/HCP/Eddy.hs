{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
module Node.HCP.Eddy
  ( rules
  , EddyUnwarpedImages (..)
  ) where

import qualified FSL
import qualified Node.HCP.Normalize     as N
import qualified Node.HCP.Preprocessing as Preprocessing
import qualified Node.HCP.Topup         as Topup
import           Node.HCP.Types         (CaseId, PhaseOrientation (..))
import           Node.HCP.Util          (hcppath)
import           Node.Util              (showKey)
import           Paths                  (outdir)
import           Shake.BuildNode

stage = "3_Eddy"

newtype EddyUnwarpedImages = EddyUnwarpedImages ([Int], CaseId)
        deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

instance BuildNode EddyUnwarpedImages where
  path n@(EddyUnwarpedImages (indices, caseid)) = hcppath caseid stage n <.> "nii.gz"

  build out@(EddyUnwarpedImages (indices, caseid)) = Just $ do
    need $ N.Dwi (N.DwiJoinedAll indices, caseid)
    need (Preprocessing.Index indices caseid)
    need (Preprocessing.AcqParams indices caseid)
    need (Topup.TopupOutput indices caseid)
    need (Topup.Mask indices caseid)
    command_ [] "eddy" ["--imain=" ++ (path $ N.Dwi (N.DwiJoinedAll indices, caseid))
                        ,"--mask=" ++ (path $ Topup.Mask indices caseid)
                        ,"--index=" ++ (path $ Preprocessing.Index indices caseid)
                        ,"--acqp=" ++ (path $ Preprocessing.AcqParams indices caseid)
                        ,"--bvecs=" ++ (FSL.bvec $ N.Dwi (N.DwiJoinedAll indices, caseid))
                        ,"--bvals=" ++ (FSL.bval $ N.Dwi (N.DwiJoinedAll indices, caseid))
                        ,"--fwhm=0"
                        ,"--topup=" ++ (hcppath caseid "2_Topup" (Topup.TopupOutput indices caseid))
                        ,"--flm=quadratic"
                        ,"-v"
                        ,"--out=" ++ path out]

rules =
  rule (buildNode :: EddyUnwarpedImages -> Maybe (Action [Double]))
