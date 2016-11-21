{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module HCP.PostEddy
  ( rules
  , HcpDwi (..)
  , NoDifBrainMask (..)
  ) where

import           Development.Shake
import           FSL               (FslDwi (..))
import           HCP.B0sPair       (B0sInfo (..), B0sPair (..))
import           HCP.Eddy          (EddyUnwarpedImages (..))
import qualified HCP.Eddy          as Eddy
import qualified HCP.Normalize     as Normalize
import qualified HCP.Preprocessing as Preprocessing
import           HCP.Types         (CaseId, PhaseOrientation (..))
import qualified HcpOutputPaths         as Paths
import           Shake.BuildNode
import           System.Directory  as IO


--------------------------------------------------------------------------------
-- HcpDwi

newtype HcpDwi = HcpDwi CaseId
        deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

instance FslDwi HcpDwi where
  nifti (HcpDwi caseid) = Paths.dataDwi_path caseid

instance BuildNode HcpDwi where
  paths dwi = [nifti dwi, bval dwi, bvec dwi]

  build out@(HcpDwi caseid) = Just $ do
    apply1 $ Eddy.EddyUnwarpedImages caseid :: Action [Double]
    apply [ Preprocessing.Series orient caseid | orient <- [Pos, Neg]]
          :: Action [[Double]]
    apply [ Preprocessing.Dwi (Just orient) caseid | orient <- [Pos, Neg]]
          :: Action [[Double]]
    b0spairs <- Normalize.getB0sPairs caseid
    let numPos = show $ sum $ map (_size._pos) b0spairs
        numNeg = show $ sum $ map (_size._neg) b0spairs
    withTempFile $ \eddypos ->
      withTempFile $ \eddyneg -> do
        command_ [] "fslroi" [(path $ EddyUnwarpedImages caseid)
                             , eddypos
                             , "0"
                             , numPos]
        command_ [] "fslroi" [(path $ EddyUnwarpedImages caseid)
                             , eddyneg
                             , numPos
                             , numNeg]
        command_ [] "eddy_combine" [ eddypos
                                   , (bval $ Preprocessing.Dwi (Just Pos) caseid)
                                   , (bvec $ Preprocessing.Dwi (Just Pos) caseid)
                                   , (path $ Preprocessing.Series Pos caseid)
                                   , eddyneg
                                   , (bval $ Preprocessing.Dwi (Just Neg) caseid)
                                   , (bvec $ Preprocessing.Dwi (Just Neg) caseid)
                                   , (path $ Preprocessing.Series Neg caseid)
                                   , (takeDirectory $ path out), "1"
                                   ]
        -- Remove negative intensity values (caused by spline interpolation) from final data
        command_ [] "fslmaths" [path out, "-thr", "0", path out]
        let outdir = takeDirectory (path out)
        liftIO $ IO.renameFile (outdir </> "bvecs") (bvec out)
        liftIO $ IO.renameFile (outdir </> "bvals") (bval out)


--------------------------------------------------------------------------------
-- NoDifBrainMask

newtype NoDifBrainMask = NoDifBrainMask CaseId
        deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

instance BuildNode NoDifBrainMask where
  path (NoDifBrainMask caseid) = Paths.dataBrainMaskPrefix_path caseid ++
    "_mask.nii.gz"

  pathPrefix (NoDifBrainMask caseid) = Paths.dataBrainMaskPrefix_path caseid

  build out@(NoDifBrainMask caseid) = Just $ do
    apply1 (HcpDwi caseid) :: Action [Double]
    command [] "bet" [(path $ HcpDwi caseid)
                     , pathPrefix out
                     , "-m"
                     , "-f"
                     , "0.1"]


rules :: Rules ()
rules = do
  rule (buildNode :: HcpDwi -> Maybe (Action [Double]))
  rule (buildNode :: NoDifBrainMask -> Maybe (Action [Double]))
