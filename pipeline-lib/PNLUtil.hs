module PNLUtil
  ( withCaseId
  -- , getConfigWithCaseId
  ) where

import           Data.List       (intercalate)
import           Data.List.Split (splitOn)

withCaseId :: String -> FilePath -> FilePath
withCaseId caseid = intercalate caseid . splitOn "{case}"

-- getConfigWithCaseId :: String -> String -> Action [FilePath]
-- getConfigWithCaseId key caseid = do
--   key' <- getConfig key
--   case key' of
--     Nothing -> error $ "Missing key in config file: " ++ key
--     Just x -> return $ map (withCaseId caseid) . words $ x
