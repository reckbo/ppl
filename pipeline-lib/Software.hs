module Software
  ( module Software.UKFTractography
  , module Software.ANTs
  , module Software.TractQuerier
  , rules
  )
  where

import qualified Software.UKFTractography
import qualified Software.ANTs
import qualified Software.TractQuerier

rules = do
  Software.UKFTractography.rules
  Software.TractQuerier.rules
  Software.ANTs.rules
