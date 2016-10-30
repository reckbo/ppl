module HCP
  (rules
  ) where

import qualified HCP.Normalize
import qualified HCP.Preprocessing
import qualified HCP.Topup
import qualified HCP.Eddy

rules = do
  HCP.Normalize.rules
  HCP.Preprocessing.rules
  HCP.Topup.rules
  HCP.Eddy.rules
