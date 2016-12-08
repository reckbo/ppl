{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
module Node.WmqlTracts
  ( rules
  , WmqlTracts (..)
   )
  where

import           Data.Foldable        (traverse_)
import qualified Development.Shake    as Shake (need)
import           Node.DWI             hiding (rules)
import           Node.DWIMask         hiding (rules)
import           Node.FreeSurfer      hiding (rules)
import           Node.TractQuerier    hiding (rules)
import           Node.UKFTractography hiding (rules)
import           Node.Util
import           Node.WmparcInDwi     hiding (rules)
import           Shake.BuildNode
import           Util                 (convertImage)

type CaseId = String

newtype WmqlTracts = WmqlTracts (FreeSurferType
                                ,FsToDwiType
                                ,DwiType
                                ,DwiMaskType
                                ,UKFTractographyType
                                ,CaseId)
                   deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

instance BuildNode WmqlTracts where
  path n@(WmqlTracts (_,_,_,_,_,caseid)) = outdir </> caseid </> showKey n </> "stamp.txt"

  build n@(WmqlTracts (fstype,fs2dwitype,dwitype,dwimasktype,ukftype,caseid)) = Just $
    withTempDir $ \tmpdir -> do
    let wmparc = WmparcInDwi (fs2dwitype, fstype, dwitype, dwimasktype, caseid)
        ukf = UKFTractography (ukftype, dwitype, dwimasktype, caseid)
        ukf_pruned = tmpdir </> "ukf_pruned.vtk"
        wmparcnii = tmpdir </> "wmparcInDwi.nii.gz"
        query = "pipeline-lib/Node/wmql-2.0.qry"
    need wmparc
    need ukf
    Shake.need [query]
    Shake.need ["config/activate_tensors.py"]
    bin <- (</> "scripts") <$> getTractQuerier
    -- Remove tracts with only 1 point
    unit $ cmd  (bin </> "tract_math") (path ukf) "tract_remove_short_tracts 2" ukf_pruned
    liftIO $ Util.convertImage (path wmparc) wmparcnii
    unit $ cmd (bin </> "tract_querier") "-t" ukf_pruned "-a" wmparcnii "-q" query
      "-o" (pathDir n)
    tracts <- liftIO $ getDirectoryFilesIO (pathDir n) ["*.vtk"]
    traverse_ (\x -> unit $ cmd "config/activate_tensors.py" x x) tracts


rules = rule (buildNode :: WmqlTracts -> Maybe (Action [Double]))
