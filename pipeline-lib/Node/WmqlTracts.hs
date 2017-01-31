{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}
module Node.WmqlTracts
  ( rules
  , WmqlTracts (..)
  )
  where

import           Data.Foldable              (traverse_)
import qualified Development.Shake          as Shake (need)
import           Node.Dwi                   hiding (rules)
import           Node.DwiMask               hiding (rules)
import           Node.FreeSurfer            hiding (rules)
import           Node.FsInDwi               hiding (rules)
import           Node.Software.TractQuerier hiding (rules)
import           Node.Types
import           Node.UKFTractography       hiding (rules)
import           Node.Util
import           Shake.BuildNode
import qualified System.Directory           as IO (renameFile)
import           System.Environment         (lookupEnv)
import           Util                       (convertImage)


data WmqlTracts =
  WmqlTracts {tqhash :: GitHash
             ,bthash :: GitHash
             ,fstype :: FreeSurferType
             ,fs2dwitype :: FsToDwiType
             ,dwitype :: DwiType
             ,dwimasktype :: DwiMaskType
             ,ukfhash :: GitHash
             ,ukftype :: UKFTractographyType
             ,caseid :: CaseId}
  deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)


instance BuildNode WmqlTracts where
  path n@(WmqlTracts{..}) = outdir </> caseid </> showKey n </> "stamp.txt"
  build n@(WmqlTracts{..}) =
    Just $
    withTempDir $
    \tmpdir ->
      do let ukf_pruned = tmpdir </> "ukf_pruned.vtk"
             wmparcnii = tmpdir </> "wmparcInDwi.nii.gz"
             query = "config/wmql-2.0.qry"
             [readme,tract_querier,tract_math] = paths TractQuerier {..}
         need FsInDwi {..}
         need UKFTractography {..}
         need TractQuerier {..}
         Shake.need [query]
         Shake.need ["config/activate_tensors.py"]
         -- TODO don't assume pythonpath is set
         Just pythonPath <- liftIO $ lookupEnv "PYTHONPATH"
         let newPythonPath = (takeDirectory readme) ++ ":" ++ pythonPath
         -- Remove tracts with only 1 point
         command_
           [AddEnv "PYTHONPATH" newPythonPath]
           tract_querier
           [path UKFTractography {..}
           ,"tract_remove_short_tracts"
           ,"2"
           ,ukf_pruned]
         liftIO $
           Util.convertImage (path FsInDwi{..})
                             wmparcnii
         command_ [AddEnv "PYTHONPATH" newPythonPath]
                  tract_querier
                  ["-t"
                  ,ukf_pruned
                  ,"-a"
                  ,wmparcnii
                  ,"-q"
                  ,query
                  ,"-o"
                  ,(pathDir n </> "_")]
         tracts <-
           liftIO $
           getDirectoryFilesIO ""
                               [pathDir n </> "*.vtk"]
         traverse_ (\f -> unit $ cmd "config/activate_tensors.py" f f) tracts
         let removePrefix f =
               replaceFileName f
                               (drop 2 . takeFileName $ f)
         traverse_ (\f ->
                      liftIO $
                      IO.renameFile f
                                    (removePrefix f))
                   tracts
         liftIO $ writeFile (path n) ""


rules = rule (buildNode :: WmqlTracts -> Maybe (Action [Double]))
