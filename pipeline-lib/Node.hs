{-# LANGUAGE RecordWildCards #-}
module Node
  ( rules
  , FreeSurfer.FreeSurfer (FreeSurfer)
  , FsInDwi.FsInDwi (FsInDwi)
  , UKFTractography.UKFTractography (UKFTractography)
  , WmqlTracts.WmqlTracts (WmqlTracts)
  , TractMeasures.TractMeasures (..)
  , Dwi.Dwi (Dwi)
  ,pathsTractMeasures
  ,pathsWmql
  )
where

import qualified Software.TractQuerier
import qualified Software.BrainsTools
import qualified Software.UKFTractography
import qualified FreeSurfer
import qualified FsInDwi
import qualified Dwi
import qualified DwiMask
import qualified T1w
import qualified T2w
import qualified T1wMask
import qualified T2wMask
import qualified Software.TractQuerier
import qualified UKFTractography
import qualified HCP
import qualified WmqlTracts
import qualified TractMeasures
import           Shake.BuildNode (path, (</>))
import Types

pathsTractMeasures :: FilePath
                   -> Int
                   -> TractMeasures.TractMeasures
                   -> [(String,FilePath)]
pathsTractMeasures projdir idx n@(TractMeasures.TractMeasures{..}) =
  [("tractmeasures" ++ show idx,projdir </> path n)] ++
  pathsWmql projdir idx WmqlTracts.WmqlTracts {..}

pathsFsInDwi projdir idx n@(FsInDwi.FsInDwi{..}) =
  let fs = FreeSurfer.FreeSurfer{..}
      dwi = Dwi.Dwi{..}
      dwimask = DwiMask.DwiMask{..}
  in [("fsindwi" ++ show idx, projdir </> path n)] ++
     pathsFs projdir idx fs ++
     pathsDwi projdir idx dwi

pathsDwi projdir idx n@(Dwi.Dwi{..}) =
  [("dwi" ++ show idx,projdir </> path n)]

pathsDwiMask projdir idx n@(DwiMask.DwiMask{..}) =
  [("dwimask" ++ show idx,projdir </> path n)]

pathsFs projdir idx n@(FreeSurfer.FreeSurfer{..}) =
  [("fs" ++ show idx,projdir </> path n)] ++ more
  where more = case fstype of
          FreeSurferGiven -> []
          (FreeSurferUsingMask t1type t1masktype) -> pathsT1w projdir idx T1w.T1w{..}

pathsT1w projdir idx n@(T1w.T1w{..}) =
  [("t1" ++ show idx,projdir </> path n)]

pathsWmql projdir idx n@(WmqlTracts.WmqlTracts{..}) =
  [("wmql" ++ show idx, projdir </> path n)] ++
  pathsFs projdir idx FreeSurfer.FreeSurfer{..} ++
  pathsDwi projdir idx Dwi.Dwi{..} ++
  pathsDwiMask projdir idx DwiMask.DwiMask{..}


rules = do
  FreeSurfer.rules
  FsInDwi.rules
  Dwi.rules
  DwiMask.rules
  T1w.rules
  T2w.rules
  T1wMask.rules
  T2wMask.rules
  UKFTractography.rules
  WmqlTracts.rules
  TractMeasures.rules
  Software.UKFTractography.rules
  Software.TractQuerier.rules
  Software.BrainsTools.rules
  HCP.rules
