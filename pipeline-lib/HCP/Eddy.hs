{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module HCP.Eddy
  ( rules
  , EddyUnwarpedImages (..)
  ) where

import           Development.Shake
import           Development.Shake.FilePath

import qualified FSL
import qualified HCP.Preprocessing          as Preprocessing
import qualified HCP.Topup                  as Topup
import           HCP.Types                  (CaseId, PhaseOrientation (..))
import qualified HcpOutputPaths                  as Paths
import           Shake.BuildKey

--------------------------------------------------------------------------------
-- EddyUnwarpedImages

newtype EddyUnwarpedImages = EddyUnwarpedImages CaseId
        deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

instance BuildKey EddyUnwarpedImages where
  path (EddyUnwarpedImages caseid) = Paths.eddyUnwarpedImages_path caseid

  build out@ (EddyUnwarpedImages caseid) = Just $ do
    apply1 $ Preprocessing.Dwi Nothing caseid :: Action [Double]
    apply1 $ Preprocessing.Index caseid :: Action [Double]
    apply1 $ Preprocessing.AcqParams caseid :: Action [Double]
    apply1 $ Topup.TopupOutput caseid :: Action [Double]
    apply1 $ Topup.NoDifBrainMask caseid :: Action [Double]
    command_ [] "eddy" ["--imain=" ++ (path $ Preprocessing.Dwi Nothing caseid)
                        ,"--mask=" ++ (path $ Topup.NoDifBrainMask caseid)
                        ,"--index=" ++ (path $ Preprocessing.Index caseid)
                        ,"--acqp=" ++ (path $ Preprocessing.AcqParams caseid)
                        ,"--bvecs=" ++ (FSL.bvec $ Preprocessing.Dwi Nothing caseid)
                        ,"--bvals=" ++ (FSL.bval $ Preprocessing.Dwi Nothing caseid)
                        ,"--fwhm=0"
                        ,"--topup=" ++ (pathPrefix $ Topup.TopupOutput caseid)
                        ,"--flm=quadratic"
                        ,"-v"
                        ,"--out=" ++ path out]

rules = do
  rule (buildKey :: EddyUnwarpedImages -> Maybe (Action [Double]))
