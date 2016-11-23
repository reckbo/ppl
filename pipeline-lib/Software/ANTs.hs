{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Software.ANTs
  ( ANTs (..)
  , rules
  , run
  )
  where

import           Data.Foldable    (traverse_)
import qualified PathsOutput      as Paths (antsPrefix)
import           Shake.BuildNode
import           Software.Util    (buildGitHubCMake)
import qualified System.Directory as IO (copyFile, createDirectoryIfMissing,
                                         removeDirectoryRecursive)
import           Development.Shake.Config

newtype ANTs = ANTs GitHash
        deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

instance BuildNode ANTs where
  paths (ANTs hash) = map (\f -> Paths.antsPrefix ++ "-" ++ hash </> f)
    ["antsRegistrationSyN.sh"
    ,"antsIntroduction.sh"
    ,"ANTS"
    ,"antsRegistration"
    ,"antsApplyTransforms"
    ,"ComposeMultiTransform"
    ]

  build out@(ANTs hash) = Just $ do
    let tmpclone = Paths.antsPrefix ++ "-" ++ hash ++ "-tmp"
    buildGitHubCMake ["-DBUILD_EXAMPLES:BOOL=OFF"
                     ,"-DBUILD_TESTING=OFF"
                     ,"-DRUN_LONG_TESTS=OFF"
                     ,"-DRUN_SHORT_TESTS=OFF"] "stnava/ANTs" hash tmpclone
    let madepaths = map (tmpclone </>)
          ["Scripts/antsRegistrationSyN.sh"
          ,"Scripts/antsIntroduction.sh"
          ,"_build/bin/ANTS"
          ,"_build/bin/antsRegistration"
          ,"_build/bin/antsApplyTransforms"
          ,"_build/bin/ComposeMultiTransform"]
    liftIO $ traverse_ (uncurry IO.copyFile) $ zip madepaths (paths out)
    liftIO $ IO.removeDirectoryRecursive tmpclone

rules = rule (buildNode :: ANTs -> Maybe (Action [Double]))

run script opts = do
  Just hash <- getConfig "ANTs-hash"
  apply1 (ANTs hash) :: Action [Double]
  command_ [AddEnv "ANTSSRC" (pathDir $ ANTs hash)
           ,AddEnv "ANTSPATH" (pathDir $ ANTs hash)]
    (pathDir (ANTs hash) </> script) opts
