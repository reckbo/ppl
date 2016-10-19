{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
module PNLPipeline
  (module Development.Shake
  ,module Development.Shake.Command
  ,module Development.Shake.FilePath
  ,module Development.Shake.Classes
  ,module Development.Shake.Rule
  ,module GHC.Generics
  ,CaseId
  ,BuildKey (..)
  ,buildKey
  ,getConfigWithCaseId
  ,withCaseId
  )
  where

import           Data.List
import           Data.Time                  (UTCTime (..), utctDayTime)
import           Development.Shake
import           Development.Shake.Classes
import           Development.Shake.Command
import           Development.Shake.FilePath
import           Development.Shake.Config
import           Development.Shake.Rule     (EqualCost (..), Rule (..), apply,
                                             apply1, rule)
import           Development.Shake.Util
import           GHC.Generics
import           System.Directory           as IO
import           Text.Printf
import Data.List.Split (splitOn)


getModTime :: FilePath -> IO Double
getModTime = fmap utcToDouble . getModificationTime
  where
    utcToDouble = fromRational . toRational . utctDayTime

type CaseId = String
type ShakeKey k  = (Generic k,Typeable k,Show k,Eq k,Hashable k,Binary k,NFData k)

class BuildKey a where
  path :: a -> FilePath
  build :: a -> Maybe (Action ())
  build _ = Nothing

instance (ShakeKey k, BuildKey k) => Rule k Double where
    storedValue _ q = do
        exists <- IO.doesFileExist $ path q
        if not exists then return Nothing
          else fmap Just $ getModTime $ path q
    equalValue _ _ old new = if old == new then EqualCheap else NotEqual

buildKey :: BuildKey a => a -> Maybe (Action Double)
buildKey k = case (build k) of
  Nothing -> Just $ liftIO $ getModTime . path $ k
  (Just action) -> Just $ do
      liftIO . createDirectoryIfMissing True . takeDirectory . path $ k
      action
      liftIO $ getModTime . path $ k


withCaseId :: String -> FilePath -> FilePath
withCaseId caseid = intercalate caseid . splitOn "{case}"

getConfigWithCaseId :: String -> String -> Action [FilePath]
getConfigWithCaseId key caseid = do
  key' <- getConfig key
  case key' of
    Nothing -> error $ "Missing key in config file: " ++ key
    Just x -> return $ map (withCaseId caseid) . words $ x
