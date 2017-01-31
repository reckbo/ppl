{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
module Node.Dwi
  (Dwi(..)
  ,rules)
  where

import           Data.List              (intercalate)
import           Data.Maybe             (fromMaybe)
import           FSL                    (bval, bvec)
import qualified Node.HCP
import           Node.HCP.B0sPair
import           Node.HCP.Eddy          hiding (rules)
import qualified Node.HCP.Normalize     as N
import qualified Node.HCP.Preprocessing as P
import           Node.HCP.Types         hiding (CaseId)
import           Node.Types
import           Node.Util              (getPath, showKey)
import           Paths
import           Shake.BuildNode
import           System.Directory       as IO (renameFile)
import           Util                   (alignAndCenterDwi)

newtype Dwi = Dwi (DwiType, CaseId)
  deriving (Show, Generic, Typeable, Eq, Hashable, Binary, NFData, Read)

instance BuildNode Dwi where
  paths (Dwi (DwiGiven, caseid)) = [getPath "dwi" caseid]
  paths n@(Dwi (DwiHcp _, caseid)) = map (basename <.>) ["nii.gz", "bval", "bvec"]
    where
      basename = outdir </> caseid </> showKey n
  paths n@(Dwi (DwiXC _, caseid)) = [outdir </> caseid </> showKey n <.> "nrrd"]

  build (Dwi (DwiGiven, _)) = Nothing

  build out@(Dwi (DwiXC dwitype, caseid)) = Just $ do
    let dwi = Dwi (dwitype, caseid)
    need dwi
    Util.alignAndCenterDwi (path dwi) (path out)

  build n@(Dwi (DwiHcp indices, caseid)) = Just $ do
    need $ EddyUnwarpedImages (indices, caseid)
    needs [ P.Series orient indices caseid
          | orient <- [Pos, Neg] ]
    needs [ N.DwiN (N.DwiJoined orient indices, caseid)
          | orient <- [Pos, Neg] ]
    b0spairs <- N.getB0sPairs caseid indices
    let numPos = show $ sum $ map (_size . _pos) b0spairs
        numNeg = show $ sum $ map (_size . _neg) b0spairs
    withTempFile $ \eddypos -> withTempFile $ \eddyneg -> do
      let outdir = pathDir n
      command_ [] "fslroi" [path $ EddyUnwarpedImages (indices, caseid), eddypos, "0", numPos]
      command_ [] "fslroi" [path $ EddyUnwarpedImages (indices, caseid), eddyneg, numPos, numNeg]
      command_ [] "eddy_combine" [eddypos, bval $ N.DwiN (N.DwiJoined Pos indices, caseid), bvec $ N.DwiN (N.DwiJoined Pos indices, caseid), path (P.Series Pos indices caseid), eddyneg, bval $ N.DwiN (N.DwiJoined Neg indices, caseid), bvec $ N.DwiN (N.DwiJoined Neg indices, caseid), path (P.Series Neg indices caseid), (pathDir n), "1"]
      -- Remove negative intensity values (caused by spline interpolation) from final data
      let [nifti', bvec', bval'] = paths n
      liftIO $ IO.renameFile (outdir </> "data.nii.gz") nifti'
      command_ [] "fslmaths" [nifti', "-thr", "0", nifti']
      liftIO $ IO.renameFile (outdir </> "bvecs") bvec'
      liftIO $ IO.renameFile (outdir </> "bvals") bval'


rules = rule (buildNode :: Dwi -> Maybe (Action [Double]))
