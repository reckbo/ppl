{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}
module Node.Dwi
  (Dwi(..)
  ,rules)
  where

import           Data.List              (intercalate)
import           Data.Maybe             (fromMaybe)
import           FSL                    (bval, bvec)
import           {-# SOURCE #-}Node.DwiMask hiding (rules)
import qualified Node.HCP
import           Node.HCP.B0sPair
import           Node.HCP.Eddy          hiding (rules)
import qualified Node.HCP.Normalize     as N
import qualified Node.HCP.Preprocessing as P
import           Node.HCP.Types         hiding (CaseId)
import           Node.T2w               hiding (rules)
import           Node.T2wMask           hiding (rules)
import           Node.Types
import           Node.Util              (getPath, showKey)
import           Paths
import           Shake.BuildNode
import           System.Directory       as IO (renameFile)
import           Util                   (alignAndCenterDwi)

data Dwi = Dwi { dwitype :: DwiType, caseid :: CaseId }
  deriving (Show, Generic, Typeable, Eq, Hashable, Binary, NFData, Read)

instance BuildNode Dwi where
  paths n@(Dwi{..}) = case dwitype of
    DwiGiven -> [getPath "dwi" caseid]
    (DwiHcp _) -> map (basename <.>) ["nii.gz","bval","bvec"]
                    where basename = outdir </> caseid </> showKey n
    _ -> [outdir </> caseid </> showKey n <.> "nrrd"]
  build out@(Dwi{..}) = case dwitype of
    DwiGiven-> Nothing
    (DwiXC srcdwitype) -> Just $ do
      let dwi = Dwi srcdwitype caseid
      need dwi
      Util.alignAndCenterDwi (path dwi) (path out)
    (DwiEpi srcdwitype dwimaskmethod t2type t2masktype) -> Just $ do
      let dwi = Dwi srcdwitype caseid
          dwimask = DwiMask dwimaskmethod srcdwitype caseid
      need dwi
      need dwimask
      need T2w{..}
      need T2wMask{..}
      command_ [] "scripts/epi.py"
        [path dwi, path dwimask, path T2w{..}, path T2wMask{..}, path out]
    (DwiHcp indices) -> Just $ do
       need $ EddyUnwarpedImages (indices,caseid)
       needs [P.Series orient indices caseid|orient <- [Pos,Neg]]
       needs [N.DwiN (N.DwiJoined orient indices,caseid)|orient <- [Pos,Neg]]
       b0spairs <- N.getB0sPairs caseid indices
       let numPos = show $ sum $ map (_size . _pos) b0spairs
           numNeg = show $ sum $ map (_size . _neg) b0spairs
       withTempFile $
         \eddypos ->
           withTempFile $
           \eddyneg ->
             do let outdir = pathDir out
                command_ []
                         "fslroi"
                         [path $ EddyUnwarpedImages (indices,caseid)
                         ,eddypos
                         ,"0"
                         ,numPos]
                command_ []
                         "fslroi"
                         [path $ EddyUnwarpedImages (indices,caseid)
                         ,eddyneg
                         ,numPos
                         ,numNeg]
                command_ []
                         "eddy_combine"
                         [eddypos
                         ,bval $ N.DwiN (N.DwiJoined Pos indices,caseid)
                         ,bvec $ N.DwiN (N.DwiJoined Pos indices,caseid)
                         ,path (P.Series Pos indices caseid)
                         ,eddyneg
                         ,bval $ N.DwiN (N.DwiJoined Neg indices,caseid)
                         ,bvec $ N.DwiN (N.DwiJoined Neg indices,caseid)
                         ,path (P.Series Neg indices caseid)
                         ,(pathDir out)
                         ,"1"]
                -- Remove negative intensity values (caused by spline interpolation) from final data
                let [nifti',bvec',bval'] = paths out
                liftIO $
                  IO.renameFile (outdir </> "data.nii.gz")
                                nifti'
                command_ [] "fslmaths" [nifti',"-thr","0",nifti']
                liftIO $
                  IO.renameFile (outdir </> "bvecs")
                                bvec'
                liftIO $
                  IO.renameFile (outdir </> "bvals")
                                bval'


rules = rule (buildNode :: Dwi -> Maybe (Action [Double]))
