{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}
module WmqlTracts
  ( rules
  , WmqlTracts (..)
  )
  where

import           Data.Foldable              (traverse_)
import qualified Development.Shake          as Shake (need)
import           Dwi                   hiding (rules)
import           DwiMask               hiding (rules)
import           FreeSurfer            hiding (rules)
import           FsInDwi               hiding (rules)
import           Software.TractQuerier hiding (rules)
import           Types
import           UKFTractography       hiding (rules)
import           NodeUtil
import           Shake.BuildNode
import qualified System.Directory           as IO (renameFile)
import           System.Environment         (lookupEnv)
import           Util                       (convertImage)


data WmqlTracts =
  WmqlTracts {tqhash        :: GitHash
             ,bthash        :: GitHash
             ,fstype        :: FreeSurferType
             ,fs2dwimethod  :: FsToDwiMethod
             ,dwimaskpair :: (DwiType, DwiMaskMethod)
             ,ukfhash       :: GitHash
             ,ukftype       :: UKFTractographyType
             ,caseid        :: CaseId}
  deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)


instance BuildNode WmqlTracts where
  path n@(WmqlTracts{..}) = outdir </> caseid </> showKey n </> "stamp.txt"
  build out@(WmqlTracts{..}) =
    Just $ do
      let [readme,tract_querier,tract_math] = paths TractQuerier {..}
          query = "config/wmql-2.0.qry"
      -- TODO don't assume pythonpath is set
      Just pythonPath <- liftIO $ lookupEnv "PYTHONPATH"
      let newPythonPath = (takeDirectory readme) ++ ":" ++ pythonPath
      Shake.need [query]
      need FsInDwi {..}
      need UKFTractography {..}
      command_ [AddEnv "PYTHONPATH" newPythonPath] "pnlscripts/wmql.py"
        ["-i", path UKFTractography{..}, "-f", path FsInDwi{..}
        ,"-q", query, "-o", pathDir out]
      liftIO $ writeFile (path out) ""


rules = rule (buildNode :: WmqlTracts -> Maybe (Action [Double]))
