{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
module Node.DWI
  ( DwiType (..)
  , Dwi (..)
  , rules
  ) where

import Util (convertDwi)
import           Data.List                  (intercalate)
import           Data.Maybe                 (fromMaybe)
import           FSL (bvec, bval)
import           Paths
import qualified Node.HCP
import           Node.HCP.B0sPair
import           Node.HCP.Eddy          hiding (rules)
import qualified Node.HCP.Normalize     as N
import qualified Node.HCP.Preprocessing as P
import           Node.HCP.Types
import           Node.Util              (showKey, getPath)
import           Shake.BuildNode
import           System.Directory           as IO (renameFile)

data DwiType = DwiGiven
             | DwiHcp [Int]
             | DwiXC DwiType
             deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

newtype Dwi = Dwi (DwiType, CaseId)
             deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

-- instance FslDwi Dwi where
--     nifti n@(Dwi (_, caseid)) = outdir </> caseid </> showKey n <.> "nii.gz"

instance BuildNode Dwi where
  paths (Dwi (DwiGiven, caseid)) = [getPath "dwi" caseid]
  paths n@(Dwi (DwiHcp _, caseid)) = map (basename <.>) ["nii.gz", "bval", "bvec"]
    where basename = outdir </> caseid </> showKey n
  paths n@(Dwi (DwiXC _, caseid)) = [outdir </> caseid </> showKey n <.> "nrrd"]

  build (Dwi (DwiGiven, _)) = Nothing

  build n@(Dwi (DwiXC dwitype, caseid)) = Just $ withTempDir $ \tmpdir -> do
    let dwiNrrd = tmpdir </> "dwiXC.nrrd"
        dwinode = Dwi (dwitype, caseid)
    need dwinode
    liftIO $ Util.convertDwi (path dwinode) dwiNrrd
    command_ [] "config/axis_align_nrrd.py" ["-i", path dwinode
                                            ,"-o", path n]
    command_ [] "config/center.py" ["-i", path n
                                   ,"-o", path n]

  build n@(Dwi (DwiHcp indices, caseid)) = Just $ do
    need $ EddyUnwarpedImages (indices, caseid)
    needs [P.Series orient indices caseid | orient <- [Pos, Neg]]
    needs [N.DwiN (N.DwiJoined orient indices, caseid) | orient <- [Pos, Neg]]
    b0spairs <- N.getB0sPairs caseid indices
    let numPos = show $ sum $ map (_size._pos) b0spairs
        numNeg = show $ sum $ map (_size._neg) b0spairs
    withTempFile $ \eddypos ->
      withTempFile $ \eddyneg -> do
        let outdir = pathDir n
        command_ [] "fslroi" [path $ EddyUnwarpedImages (indices, caseid)
                             , eddypos
                             , "0"
                             , numPos]
        command_ [] "fslroi" [path $ EddyUnwarpedImages (indices, caseid)
                             , eddyneg
                             , numPos
                             , numNeg]
        command_ [] "eddy_combine" [ eddypos
                                   , bval $ N.DwiN (N.DwiJoined Pos indices, caseid)
                                   , bvec $ N.DwiN (N.DwiJoined Pos indices, caseid)
                                   , path (P.Series Pos indices caseid)
                                   , eddyneg
                                   , bval $ N.DwiN (N.DwiJoined Neg indices, caseid)
                                   , bvec $ N.DwiN (N.DwiJoined Neg indices, caseid)
                                   , path (P.Series Neg indices caseid)
                                   , (pathDir n), "1"
                                   ]
        -- Remove negative intensity values (caused by spline interpolation) from final data
        let [nifti', bvec', bval'] = paths n
        liftIO $ IO.renameFile (outdir </> "data.nii.gz") nifti'
        command_ [] "fslmaths" [nifti', "-thr", "0", nifti']
        liftIO $ IO.renameFile (outdir </> "bvecs") bvec'
        liftIO $ IO.renameFile (outdir </> "bvals") bval'


-- DWIConvert --conversionMode FSLToNrrd --inputBVectors data-1.bvec --inputBValues data-1.bval --fslNIFTIFile data-1.nii.gz -o data-1.nrrd

rules = rule (buildNode :: Dwi -> Maybe (Action [Double]))
