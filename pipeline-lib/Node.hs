{-# LANGUAGE RecordWildCards #-}
module Node
  ( rules
  , Node.FreeSurfer.FreeSurfer (FreeSurfer)
  , Node.FsInDwi.FsInDwi (FsInDwi)
  , Node.UKFTractography.UKFTractography (UKFTractography)
  , Node.WmqlTracts.WmqlTracts (WmqlTracts)
  , Node.TractMeasures.TractMeasures (..)
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
import qualified Node.Structural
import qualified Node.StructuralMask
import qualified Node.Software.TractQuerier
import qualified Node.UKFTractography
import qualified Node.HCP
import qualified Node.WmqlTracts
import qualified Node.TractMeasures
import           Shake.BuildNode (path, (</>))
import Node.Types (StructuralType (..))

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
  [("fs" ++ show idx,projdir </> path n)] ++ pathsT1w projdir idx t1w
  where t1w = Node.Structural.Structural T1w caseid

pathsT1w projdir idx n@(Node.Structural.Structural{..}) =
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
  Node.Structural.rules
  Node.StructuralMask.rules
  Node.UKFTractography.rules
  Node.WmqlTracts.rules
  Node.TractMeasures.rules
  Node.Software.UKFTractography.rules
  Node.Software.TractQuerier.rules
  Node.Software.BrainsTools.rules
  Node.HCP.rules
