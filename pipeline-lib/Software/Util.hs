module Software.Util
  (
    buildGitHubCMake
  ) where

import           Control.Monad              (unless, when)
import           Development.Shake          (Action, CmdOption (Cwd), cmd,
                                             liftIO)
import           Development.Shake.FilePath ((</>))
import qualified System.Directory           as IO

buildGitHubCMake :: [String] -> String -> String -> FilePath -> Action ()
buildGitHubCMake cmakeopts githubAddress hash clonedir = do
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
    cmd [Cwd builddir] "cmake" cmakeopts clonedirAbs :: Action ()
    cmd [Cwd builddir] "make -j6" :: Action ()
