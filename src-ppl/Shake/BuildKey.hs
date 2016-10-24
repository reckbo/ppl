{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
module Shake.BuildKey
  (module Development.Shake
  ,module Development.Shake.Command
  ,module Development.Shake.FilePath
  ,module Development.Shake.Classes
  ,module Development.Shake.Rule
  ,module GHC.Generics
  ,BuildKey (..)
  ,buildKey
  )
  where

import           Data.Time                  (UTCTime (..), utctDayTime)
import           Development.Shake
import           Development.Shake.Classes
import           Development.Shake.Command
import           Development.Shake.Config
import           Development.Shake.FilePath
import           Development.Shake.Rule     (EqualCost (..), Rule (..), apply,
                                             apply1, rule)
import           Development.Shake.Util
import           GHC.Generics
import           System.Directory           as IO
import           Text.Printf


getModTime :: FilePath -> IO Double
getModTime = fmap utcToDouble . getModificationTime
  where
    utcToDouble = fromRational . toRational . utctDayTime

type CaseId = String
type ShakeKey k  = (Generic k,Typeable k,Show k,Eq k,Hashable k,Binary k,NFData k)

class BuildKey a where
  paths :: a -> [FilePath]

  build :: a -> Maybe (Action ())
  build _ = Nothing

instance (ShakeKey k, BuildKey k) => Rule k [Double] where
    storedValue _ q = do
        exists <- traverse IO.doesFileExist $ paths q
        if not (and exists) then return Nothing
        else fmap Just $ traverse getModTime $ paths q
    equalValue _ _ old new = if old == new then EqualCheap else NotEqual

buildKey :: BuildKey a => a -> Maybe (Action [Double])
buildKey k = case (build k) of
  Nothing -> Just $ liftIO $ traverse getModTime $ paths k -- No action, source node
  (Just action) -> Just $ do
      liftIO $ traverse (createDirectoryIfMissing True) $ map takeDirectory . paths $ k
      action
      liftIO $ traverse getModTime $ paths k
