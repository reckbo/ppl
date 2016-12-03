{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
module Pipeline.DWIMask
  ( DwiMaskType (..)
  , rules
  ) where

import           Data.Maybe       (fromMaybe)
import qualified Paths
import           Pipeline.DWI     hiding (rules)
import           Pipeline.Util    (showKey)
import           Shake.BuildNode
import           System.Directory as IO (renameFile)

type CaseId = String

data DwiMaskType = DwiMaskSource
                 | DwiMaskHcp
        deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

instance BuildNode (DwiMaskType, DwiType, CaseId) where
  path (DwiMaskSource, _, caseid) = fromMaybe
    (error "Set 'dwimask' in Paths.hs") $ Paths.dwimask caseid
  path k@(DwiMaskHcp, _, caseid) = Paths.hcpdir caseid "4_Data" </>
                                 showKey k <.> "nii.gz"

  build (DwiMaskSource,_,_) = Nothing

  build key@(DwiMaskHcp, dwitype, caseid) = Just $ do
    need $ Dwi (dwitype, caseid)
    unit $ command [] "bet" [path $ Dwi (dwitype, caseid)
                            , caseid
                            , "-m"
                            , "-f"
                            , "0.1"]
    liftIO $ IO.renameFile (caseid ++ "_mask.nii.gz") (path key)


rules = rule (buildNode :: (DwiMaskType, DwiType, CaseId) -> Maybe (Action [Double]))
