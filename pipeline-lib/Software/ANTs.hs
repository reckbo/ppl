{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Software.ANTs
  (
    ANTs (..)
  , rules
  )
  where

import           Control.Monad       (unless, when)
import           Data.Foldable
import           Shake.BuildNode
import qualified SoftwareOutputPaths as Paths (antsPrefix)
import qualified System.Directory    as IO

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
    buildGitHubCMake "stnava/ANTs" hash tmpclone
    let madepaths = map (tmpclone </>)
          ["Scripts/antsRegistrationSyN.sh"
          ,"Scripts/antsIntroduction.sh"
          ,"_build/bin/ANTS"
          ,"_build/bin/antsRegistration"
          ,"_build/bin/antsApplyTransforms"
          ,"_build/bin/ComposeMultiTransform"]
    traverse_ (uncurry copyFile') $ zip madepaths (paths out)
    liftIO $ IO.removeDirectoryRecursive tmpclone


buildGitHubCMake githubAddress hash clonedir = do
    -- Checkout repo
    cloneExists <- liftIO $ IO.doesDirectoryExist clonedir
    cmakeListsExists <- liftIO $ IO.doesFileExist (clonedir </> "CMakeLists.txt")
    unless cmakeListsExists
      (do
          liftIO $ when cloneExists $ IO.removeDirectoryRecursive clonedir
          cmd "git clone" ("https://github.com/" ++ githubAddress) clonedir :: Action ()
      )
    cmd [Cwd clonedir] "git checkout" hash :: Action ()

    -- Build
    clonedirAbs <- liftIO $ IO.makeAbsolute $ clonedir
    let builddir = clonedirAbs </> "_build"
    liftIO $ IO.createDirectoryIfMissing False builddir
    cmd [Cwd builddir] "cmake" clonedirAbs :: Action ()
    cmd [Cwd builddir] "make -j6" :: Action ()


rules = rule (buildNode :: ANTs -> Maybe (Action [Double]))
