module Pipeline
  ( rules
  , Pipeline.FsInDwi.FsInDwiType (..)
  , Pipeline.ANTs.ANTs (..)
  , Pipeline.FreeSurfer.FreeSurferType (..)
  , Pipeline.DWI.DwiType (..)
  , Pipeline.DWIMask.DwiMaskType (..)
  , Pipeline.Structural.StructuralType (..)
  , Pipeline.StructuralMask.StructuralMaskType (..)
  , Pipeline.TractQuerier.TractQuerier (..)
  , Pipeline.UKFTractography.UKFTractographyExe (..)
  , Pipeline.UKFTractography.UKFTractographyType (..)
  ) where

import qualified Pipeline.ANTs
import qualified Pipeline.FreeSurfer
import qualified Pipeline.DWI
import qualified Pipeline.DWIMask
import qualified Pipeline.FsInDwi
import qualified Pipeline.Structural
import qualified Pipeline.StructuralMask
import qualified Pipeline.TractQuerier
import qualified Pipeline.UKFTractography

rules = do
  Pipeline.ANTs.rules
  Pipeline.FreeSurfer.rules
  Pipeline.DWI.rules
  Pipeline.DWIMask.rules
  Pipeline.FsInDwi.rules
  Pipeline.Structural.rules
  Pipeline.StructuralMask.rules
  Pipeline.TractQuerier.rules
  Pipeline.UKFTractography.rules