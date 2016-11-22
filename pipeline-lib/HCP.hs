module HCP
  ( HCP.PostEddy.HcpDwi (..)
  , rules
  ) where

import qualified HCP.Normalize
import qualified HCP.Preprocessing
import qualified HCP.Topup
import qualified HCP.Eddy
import qualified HCP.PostEddy

rules = do
  HCP.Normalize.rules
  HCP.Preprocessing.rules
  HCP.Topup.rules
  HCP.Eddy.rules
  HCP.PostEddy.rules
