module Util
  (convertImage
  ,maskImage
  ,buildGitHubCMake
  ) where

import           Control.Monad    (unless, when)
import qualified FSL              (isNifti, mask)
import qualified System.Directory as IO (copyFile, createDirectoryIfMissing,
                                         doesDirectoryExist, doesFileExist,
                                         makeAbsolute, removeDirectoryRecursive)
import           System.FilePath  (takeExtensions, (<.>), (</>))
import           System.IO.Temp   (withSystemTempDirectory)
import           System.Process   (CreateProcess (..), callProcess,
                                   createProcess, proc)
import qualified Teem             (isNrrd, mask)

convertImage :: FilePath -> FilePath -> IO ()
convertImage infile outfile
  = if takeExtensions infile == takeExtensions outfile then
      IO.copyFile infile outfile
    else
      callProcess "ConvertBetweenFileFormats" [infile, outfile]

maskImage :: FilePath -> FilePath -> FilePath -> IO ()
maskImage mask img out | FSL.isNifti out = maskImageUsing "nii.gz" FSL.mask out
                       | Teem.isNrrd out = maskImageUsing "nrrd" Teem.mask out
                       | otherwise = error "maskImage: images must be in Nrrd or Nifti format"
                       where
                         maskImageUsing ext maskFn out
                           = withSystemTempDirectory "maskImage" $ \tmpdir -> do
                           let tmpmask = tmpdir </> "mask.nii.gz"
                               tmpimg = tmpdir </> "tmpimg.nii.gz"
                           convertImage mask tmpmask
                           convertImage img tmpimg
                           maskFn tmpimg tmpmask out

buildGitHubCMake :: [String] -> String -> String -> FilePath -> IO ()
buildGitHubCMake cmakeopts githubAddress hash clonedir = do
    -- Checkout repo
    cloneExists <- IO.doesDirectoryExist clonedir
    cmakeListsExists <- IO.doesFileExist (clonedir </> "CMakeLists.txt")
    unless cmakeListsExists
      (do
          when cloneExists $ IO.removeDirectoryRecursive clonedir
          callProcess "git" ["clone"
                            ,("https://github.com/" ++ githubAddress)
                            ,clonedir]
      )
    createProcess $ (proc "git" ["checkout", hash]) {cwd = Just clonedir}
    -- Build
    clonedirAbs <- IO.makeAbsolute $ clonedir
    let builddir = clonedirAbs </> "_build"
    IO.createDirectoryIfMissing False builddir
    createProcess $ (proc "cmake" $ cmakeopts++[clonedirAbs]){cwd = Just builddir}
    createProcess $ (proc "make" []){cwd = Just builddir}
    return ()
