module BuildNode.HCP
  ( BuildNode.HCP.PostEddy.HcpDwi (..)
  , rules
  ) where

import qualified BuildNode.HCP.Normalize
import qualified BuildNode.HCP.Preprocessing
import qualified BuildNode.HCP.Topup
import qualified BuildNode.HCP.Eddy
import qualified BuildNode.HCP.PostEddy

rules = do
  BuildNode.HCP.Normalize.rules
  BuildNode.HCP.Preprocessing.rules
  BuildNode.HCP.Topup.rules
  BuildNode.HCP.Eddy.rules
  BuildNode.HCP.PostEddy.rules
