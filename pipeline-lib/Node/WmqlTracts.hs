{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
module Node.WmqlTracts
  ( rules
  , WmqlTracts (..)
  )
  where

import System.Environment (lookupEnv)
import           Data.Foldable        (traverse_)
import qualified Development.Shake    as Shake (need)
import           Node.DWI             hiding (rules)
import           Node.DWIMask         hiding (rules)
import           Node.FreeSurfer      hiding (rules)
import           Node.TractQuerier    hiding (rules)
import           Node.UKFTractography hiding (rules)
import           Node.Util
import           Node.WmparcInDwi     hiding (rules)
import           Shake.BuildNode
import           Util                 (convertImage)
import qualified System.Directory as IO (renameFile)

type CaseId = String

data WmqlTracts = WmqlTracts { fstype :: FreeSurferType
                              ,fs2dwitype :: FsToDwiType
                              ,dwitype :: DwiType
                              ,dwimasktype :: DwiMaskType
                              ,ukftype :: UKFTractographyType
                              ,caseid :: CaseId }
                   deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)


instance BuildNode WmqlTracts where
  -- path n@(WmqlTracts (_,_,_,_,_,caseid)) = outdir </> caseid </> showKey n </> "stamp.txt"
  path n@(WmqlTracts _ _ _ _ _ caseid) = outdir </> caseid </> showKey n </> "stamp.txt"

  build n@(WmqlTracts{..}) = Just $
    withTempDir $ \tmpdir -> do
    let wmparc = WmparcInDwi (fs2dwitype, fstype, dwitype, dwimasktype, caseid)
        ukf = UKFTractography (ukftype, dwitype, dwimasktype, caseid)
        ukf_pruned = tmpdir </> "ukf_pruned.vtk"
        wmparcnii = tmpdir </> "wmparcInDwi.nii.gz"
        query = "config/wmql-2.0.qry"
    need wmparc
    need ukf
    Shake.need [query]
    Shake.need ["config/activate_tensors.py"]
    repo <- getTractQuerier
    -- TODO don't assume pythonpath is set
    Just pythonPath <- liftIO $ lookupEnv "PYTHONPATH"
    let newPythonPath = repo ++ ":" ++ pythonPath
    -- Remove tracts with only 1 point
    command_  [AddEnv "PYTHONPATH" newPythonPath] (repo </> "scripts/tract_math")
      [path ukf
      ,"tract_remove_short_tracts" ,"2"
      , ukf_pruned]
    liftIO $ Util.convertImage (path wmparc) wmparcnii
    command_ [AddEnv "PYTHONPATH" newPythonPath] (repo </> "scripts/tract_querier")
      ["-t", ukf_pruned
      ,"-a", wmparcnii
      ,"-q", query
      ,"-o", (pathDir n </> "_")
      ]
    tracts <- liftIO $ getDirectoryFilesIO "" [pathDir n </> "*.vtk"]
    case length tracts of
        0 -> error "WmqlTracts: No tracts extracted"
        _ -> do
                traverse_ (\f -> unit $ cmd "config/activate_tensors.py" f f) tracts
                let removePrefix f = replaceFileName f (drop 2 . takeFileName $ f)
                traverse_  (\f -> liftIO $ IO.renameFile f (removePrefix f)) tracts
                liftIO $ writeFile (path n) ""


rules = rule (buildNode :: WmqlTracts -> Maybe (Action [Double]))
