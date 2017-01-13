{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Node.TractQuerier
  (TractQuerier (..)
  ,getTractQuerier
  ,rules
  ) where

-- import           Control.Monad             (unless)
-- import           Data.Foldable             (traverse_)
import           Node.Util (showKey)
import           Paths                     (outdir, softwareDir)
import           Shake.BuildNode
-- import qualified System.Directory          as IO (removeDirectoryRecursive)
-- import           System.Directory.PathWalk (pathWalk)


newtype TractQuerier = TractQuerier GitHash
        deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

instance BuildNode TractQuerier where
  path n = softwareDir </> showKey n </> "README.md"
  build _ = Nothing


-- instance GithubNode TractQuerier where
--   gitHash (TractQuerier hash) = hash

--   githubAddress _ = "demianw/tract_querier.git"

--   cloneDir n@(TractQuerier hash) = outdir </> showKey n

--   buildRepo out@(TractQuerier hash) = Just $ do
--     -- saves 70 MB of space
--     liftIO $ IO.removeDirectoryRecursive (cloneDir out </> "doc")
--     liftIO $ IO.removeDirectoryRecursive (cloneDir out </> ".git")


getTractQuerier :: Action FilePath
getTractQuerier = do
  Just n <- fmap TractQuerier <$> getConfig "tract_querier-hash"
  need n
  return $ takeDirectory $ path n

-- rules = rule (buildGithubNode :: TractQuerier -> Maybe (Action String))
rules = rule (buildNode :: TractQuerier -> Maybe (Action [Double]))
