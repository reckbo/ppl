module HCP
  ( module HCP.Normalize
  , module HCP.Preprocessing
  , rules
  ) where

import qualified HCP.Normalize
import qualified HCP.Preprocessing
import qualified HCP.Topup1

rules = do
  HCP.Normalize.rules
  HCP.Preprocessing.rules
  HCP.Topup1.rules