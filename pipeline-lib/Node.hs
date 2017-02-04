{-# LANGUAGE RecordWildCards #-}
module Node
  ( rules
  , Node.FreeSurfer.FreeSurfer (FreeSurfer)
  , Node.FsInDwi.FsInDwi (FsInDwi)
  , Node.UKFTractography.UKFTractography (UKFTractography)
  , Node.WmqlTracts.WmqlTracts (WmqlTracts)
  , Node.TractMeasures.TractMeasures (..)
  , Node.Dwi.Dwi (Dwi)
  ,pathsTractMeasures
  ,pathsWmql
  )
where

import qualified Node.Software.TractQuerier
import qualified Node.Software.BrainsTools
import qualified Node.Software.UKFTractography
import qualified Node.FreeSurfer
import qualified Node.FsInDwi
import qualified Node.Dwi
import qualified Node.DwiMask
import qualified Node.T1w
import qualified Node.T2w
import qualified Node.T1wMask
import qualified Node.T2wMask
import qualified Node.Software.TractQuerier
import qualified Node.UKFTractography
import qualified Node.HCP
import qualified Node.WmqlTracts
import qualified Node.TractMeasures
import           Shake.BuildNode (path, (</>))
import Node.Types

pathsTractMeasures :: FilePath
                   -> Int
                   -> Node.TractMeasures.TractMeasures
                   -> [(String,FilePath)]
pathsTractMeasures projdir idx n@(Node.TractMeasures.TractMeasures{..}) =
  [("tractmeasures" ++ show idx,projdir </> path n)] ++
  pathsWmql projdir idx Node.WmqlTracts.WmqlTracts {..}

pathsFsInDwi projdir idx n@(Node.FsInDwi.FsInDwi{..}) =
  let fs = Node.FreeSurfer.FreeSurfer{..}
      dwi = Node.Dwi.Dwi{..}
      dwimask = Node.DwiMask.DwiMask{..}
  in [("fsindwi" ++ show idx, projdir </> path n)] ++
     pathsFs projdir idx fs ++
     pathsDwi projdir idx dwi

pathsDwi projdir idx n@(Node.Dwi.Dwi{..}) =
  [("dwi" ++ show idx,projdir </> path n)]

pathsDwiMask projdir idx n@(Node.DwiMask.DwiMask{..}) =
  [("dwimask" ++ show idx,projdir </> path n)]

pathsFs projdir idx n@(Node.FreeSurfer.FreeSurfer{..}) =
  [("fs" ++ show idx,projdir </> path n)] ++ more
  where more = case fstype of
          FreeSurferGiven -> []
          (FreeSurferUsingMask t1type t1masktype) -> pathsT1w projdir idx Node.T1w.T1w{..}

pathsT1w projdir idx n@(Node.T1w.T1w{..}) =
  [("t1" ++ show idx,projdir </> path n)]

pathsWmql projdir idx n@(Node.WmqlTracts.WmqlTracts{..}) =
  [("wmql" ++ show idx, projdir </> path n)] ++
  pathsFs projdir idx Node.FreeSurfer.FreeSurfer{..} ++
  pathsDwi projdir idx Node.Dwi.Dwi{..} ++
  pathsDwiMask projdir idx Node.DwiMask.DwiMask{..}


rules = do
  Node.FreeSurfer.rules
  Node.FsInDwi.rules
  Node.Dwi.rules
  Node.DwiMask.rules
  Node.T1w.rules
  Node.T2w.rules
  Node.T1wMask.rules
  Node.T2wMask.rules
  Node.UKFTractography.rules
  Node.WmqlTracts.rules
  Node.TractMeasures.rules
  Node.Software.UKFTractography.rules
  Node.Software.TractQuerier.rules
  Node.Software.BrainsTools.rules
  Node.HCP.rules
