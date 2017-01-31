{-# LANGUAGE RecordWildCards #-}
module Node
  ( rules
  , Node.ANTs.ANTs (..)
  , Node.FreeSurfer.FreeSurfer (..)
  , Node.WmparcInDwi.WmparcInDwi (..)
  , Node.WmparcInDwi.FsToDwiType (..)
  , Node.TractQuerier.TractQuerier (..)
  , Node.UKFTractography.UKFTractographyExe (..)
  , Node.UKFTractography.UKFTractography (..)
  , Node.WmqlTracts.WmqlTracts (WmqlTracts)
  , Node.MeasureTracts.MeasureTracts (..)
  , Node.MeasureTractsCsv.MeasureTractsCsv (MeasureTractsCsv)
  ,pathsMeasureTracts
  ,pathsWmql
  )
where

import qualified Node.ANTs
import qualified Node.FreeSurfer
import qualified Node.WmparcInDwi
import qualified Node.DWI
import qualified Node.DWIMask
import qualified Node.Structural
import qualified Node.StructuralMask
import qualified Node.TractQuerier
import qualified Node.UKFTractography
import qualified Node.HCP
import qualified Node.WmqlTracts
import qualified Node.MeasureTracts
import qualified Node.MeasureTractsCsv
import           Shake.BuildNode (path, (</>))

pathsMeasureTracts :: FilePath
                   -> Int
                   -> Node.MeasureTractsCsv.MeasureTractsCsv
                   -> [(String,FilePath)]
pathsMeasureTracts projdir idx n@(Node.MeasureTractsCsv.MeasureTractsCsv{..}) =
  let wmql = Node.WmqlTracts.WmqlTracts {..}
  in [("measuretracts" ++ show idx,projdir </> path n)] ++
     pathsWmql projdir idx wmql

pathsWmql :: FilePath -> Int -> Node.WmqlTracts.WmqlTracts -> [(String, FilePath)]
pathsWmql projdir idx n = [("wmql" ++ show idx, projdir </> path n)]



rules = do
  Node.ANTs.rules
  Node.FreeSurfer.rules
  Node.WmparcInDwi.rules
  Node.DWI.rules
  Node.DWIMask.rules
  Node.Structural.rules
  Node.StructuralMask.rules
  Node.TractQuerier.rules
  Node.UKFTractography.rules
  Node.WmqlTracts.rules
  Node.MeasureTracts.rules
  Node.MeasureTractsCsv.rules
  Node.HCP.rules
