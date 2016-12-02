{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
module Pipeline.DWI
  ( DwiType (..)
  , rules
  ) where

import           Data.List                  (intercalate)
import           Data.Maybe                 (fromMaybe)
import           FSL
import           Paths
import qualified Pipeline.HCP
import           Pipeline.HCP.B0sPair
import Pipeline.HCP.Eddy          hiding (rules)
import qualified Pipeline.HCP.Normalize     as N
import qualified Pipeline.HCP.Preprocessing as P
import           Pipeline.HCP.Types
import           Pipeline.Util              (showKey)
import           Shake.BuildNode
import           System.Directory           as IO (renameFile)

data DwiType = DwiSource
             | DwiXc
             | DwiHcp [Int]
             deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

instance FslDwi (DwiType, CaseId) where
    nifti key@(_, caseid) = outdir </> caseid </> keyToString key <.> "nii.gz"
                                    where keyToString (DwiHcp xs, caseid) =
                                            intercalate "-" $ ["DwiHcp"] ++ map show xs ++ [caseid]

instance BuildNode (DwiType, CaseId) where
  paths key@(DwiHcp _, caseid) = [nifti key, bval key, bvec key]
  paths (DwiSource, caseid) = fromMaybe (error "Set 'dwi' in Paths.hs") $
                                        fmap (:[]) $ Paths.dwi caseid -- TODO assumes nrrd
  paths k@(_, caseid) = [outdir </> caseid </> showKey k <.> "nrrd"]

  build (DwiSource, _) = Nothing

  build key@(DwiXc, caseid) = Just $ do
    need (DwiSource, caseid)
    command_ [] "config/axis_align_nrrd.py" ["-i", path (DwiSource, caseid)
                                            ,"-o", path key]
    command_ [] "config/center.py" ["-i", path key
                                   ,"-o", path key]

  build key@(DwiHcp indices, caseid) = Just $ do
    need $ EddyUnwarpedImages (indices, caseid)
    needs [P.Series orient indices caseid | orient <- [Pos, Neg]]
    needs [N.Dwi (N.DwiJoined orient indices, caseid) | orient <- [Pos, Neg]]
    b0spairs <- N.getB0sPairs caseid indices
    let numPos = show $ sum $ map (_size._pos) b0spairs
        numNeg = show $ sum $ map (_size._neg) b0spairs
    withTempFile $ \eddypos ->
      withTempFile $ \eddyneg -> do
        let outdir = pathDir key
        command_ [] "fslroi" [path $ EddyUnwarpedImages (indices, caseid)
                             , eddypos
                             , "0"
                             , numPos]
        command_ [] "fslroi" [path $ EddyUnwarpedImages (indices, caseid)
                             , eddyneg
                             , numPos
                             , numNeg]
        command_ [] "eddy_combine" [ eddypos
                                   , bval $ N.Dwi (N.DwiJoined Pos indices, caseid)
                                   , bvec $ N.Dwi (N.DwiJoined Pos indices, caseid)
                                   , path (P.Series Pos indices caseid)
                                   , eddyneg
                                   , bval $ N.Dwi (N.DwiJoined Neg indices, caseid)
                                   , bvec $ N.Dwi (N.DwiJoined Neg indices, caseid)
                                   , path (P.Series Neg indices caseid)
                                   , (pathDir key), "1"
                                   ]
        -- Remove negative intensity values (caused by spline interpolation) from final data
        liftIO $ IO.renameFile (outdir </> "data.nii.gz") (nifti key) 
        command_ [] "fslmaths" [nifti key, "-thr", "0", nifti key]
        liftIO $ IO.renameFile (outdir </> "bvecs") (bvec key)
        liftIO $ IO.renameFile (outdir </> "bvals") (bval key)


-- DWIConvert --conversionMode FSLToNrrd --inputBVectors data-1.bvec --inputBValues data-1.bval --fslNIFTIFile data-1.nii.gz -o data-1.nrrd

rules = rule (buildNode :: (DwiType, CaseId) -> Maybe (Action [Double]))
