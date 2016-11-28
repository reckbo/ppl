module Util
  (convertImage
  ,maskImage
  ,buildGitHubCMake
  ,keyToString
  ,keyToString3
  ,keyToString4
  ,keyToString5
  ,keyToString6
  ,extractB0
  ) where

import           Control.Monad.Extra    (unlessM, whenM)
import           Control.Monad    (unless, when)
import qualified FSL              (isNifti, mask, extractB0)
import qualified Teem             (isNrrd, mask, extractB0)
import qualified System.Directory as IO (copyFile, createDirectoryIfMissing,
                                         doesDirectoryExist, doesFileExist,
                                         makeAbsolute, removeDirectoryRecursive)
import           System.FilePath  (takeExtensions, (<.>), (</>))
import           System.IO.Temp   (withSystemTempDirectory)
import           System.Process   (CreateProcess (..), callProcess,
                                   createProcess, proc)
import Shake.BuildNode

keyToString :: (Show a, Show b) => (a, b) -> String
keyToString (a,b) = show a ++ "-" ++ show b

keyToString3 :: (Show a, Show b, Show c) => (a, b, c) -> String
keyToString3 (a,b,c) = show a ++ "-" ++ show b ++ "-" ++ show c

keyToString4 :: (Show a, Show b, Show c, Show d) => (a, b, c, d) -> String
keyToString4 (a,b,c,d) = show a ++ "-" ++ show b ++ "-" ++ show c ++ "-" ++ show d

keyToString5 :: (Show a, Show b, Show c, Show d, Show e) => (a, b, c, d, e) -> String
keyToString5 (a,b,c,d,e) = show a ++ "-" ++ show b ++ "-" ++ show c ++ "-" ++ show d ++ "-" ++ show e

keyToString6 :: (Show a, Show b, Show c, Show d, Show e, Show f) => (a, b, c, d, e, f) -> String
keyToString6 (a,b,c,d,e,f) = show a ++ "-" ++ show b ++ "-" ++ show c ++ "-" ++ show d ++ "-" ++ show e ++ "-" ++ show f

convertImage :: FilePath -> FilePath -> IO ()
convertImage infile outfile
  = if takeExtensions infile == takeExtensions outfile then
      IO.copyFile infile outfile
    else
      callProcess "ConvertBetweenFileFormats" [infile, outfile]

maskImage :: FilePath -> FilePath -> FilePath -> IO ()
maskImage img mask out | FSL.isNifti out = maskImageUsing "nii.gz" FSL.mask out
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

extractB0 :: FilePath -> FilePath -> IO ()
extractB0 out dwi | FSL.isNifti out = FSL.extractB0 out dwi
                  | Teem.isNrrd out = Teem.extractB0 out dwi
                  | otherwise = error "extractB0: Invalid dwi, must be nrrd or nifti"

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
