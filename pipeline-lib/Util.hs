module Util
  (convertImage
  ,maskImage
  ,buildGitHubCMake
  ,keyToString
  ,keyToString3
  ,keyToString4
  ) where

import           Control.Monad.Extra    (unlessM, whenM)
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
import Shake.BuildNode

keyToString :: (Show a, Show b) => (a, b) -> String
keyToString (a,b) = show a ++ "-" ++ show b

keyToString3 :: (Show a, Show b, Show c) => (a, b, c) -> String
keyToString3 (a,b,c) = show a ++ "-" ++ show b ++ "-" ++ show c

keyToString4 :: (Show a, Show b, Show c, Show d) => (a, b, c, d) -> String
keyToString4 (a,b,c,d) = show a ++ "-" ++ show b ++ "-" ++ show c ++ "-" ++ show d

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

buildGitHubCMake :: [String] -> String -> String -> FilePath -> Action ()
buildGitHubCMake cmakeopts githubAddress hash clonedir = do
    liftIO $ whenM (not <$> IO.doesFileExist (clonedir </> "CMakeLists.txt"))
      (do
          whenM (IO.doesDirectoryExist clonedir)
            $ IO.removeDirectoryRecursive clonedir
          callProcess "git" ["clone"
                            ,("https://github.com/" ++ githubAddress)
                            ,clonedir]
      )
    -- createProcess $ (proc "git" ["checkout", hash]) {cwd = Just clonedir}
    unit $ cmd (Cwd clonedir) "git checkout" hash
    -- Build
    clonedirAbs <- liftIO $ IO.makeAbsolute $ clonedir
    let builddir = clonedirAbs </> "_build"
    liftIO $ IO.createDirectoryIfMissing False builddir
    unit $ cmd (Cwd builddir) "cmake" (cmakeopts++[clonedirAbs])
    unit $ cmd (Cwd builddir) "make"
    -- createProcess $ (proc "cmake" $ cmakeopts++[clonedirAbs]){cwd = Just builddir}
    -- createProcess $ (proc "make" []){cwd = Just builddir}
    -- return ()
