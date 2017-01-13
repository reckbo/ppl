{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Node.MeasureTracts
  (rules
  ,MeasureTracts (..)
  ,getMeasureTracts
  ) where

import           Node.Util
import           Shake.BuildNode
-- import qualified System.Directory           as IO (removeDirectoryRecursive)
import Paths (softwareDir)

newtype MeasureTracts = MeasureTracts GitHash
        deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

instance BuildNode MeasureTracts where
  path n = softwareDir </> showKey n </> "README"
-- instance GithubNode MeasureTracts where
--   gitHash (MeasureTracts hash) = hash

--   githubAddress _ = "pnlbwh/measuretracts"

--   cloneDir n@(MeasureTracts hash) = outdir </> showKey n

--   buildRepo out@(MeasureTracts hash) = Just $ do
--     -- to save some space
--     liftIO $ IO.removeDirectoryRecursive (cloneDir out </> ".git")


getMeasureTracts :: Action FilePath
getMeasureTracts = do
  Just n <- fmap MeasureTracts <$> getConfig "measuretracts-hash"
  need n
  return $ takeDirectory . path $ n

-- rules = rule (buildGithubNode :: MeasureTracts -> Maybe (Action String))
rules = rule (buildNode :: MeasureTracts -> Maybe (Action [Double]))
