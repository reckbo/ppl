module Node.HCP
  ( -- Node.HCP.PostEddy.HcpDwi (..)
    rules
  ) where

import qualified Node.HCP.Normalize
import qualified Node.HCP.Preprocessing
import qualified Node.HCP.Topup
import qualified Node.HCP.Eddy
-- import qualified Node.HCP.PostEddy

rules = do
  Node.HCP.Normalize.rules
  Node.HCP.Preprocessing.rules
  Node.HCP.Topup.rules
  Node.HCP.Eddy.rules
  -- Node.HCP.PostEddy.rules
