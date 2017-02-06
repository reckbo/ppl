module Util
  (convertImage
  ,convertDwi
  ,maskImage
  ,buildGitHubCMake
  ,extractB0
  ,alignAndCenter
  ,alignAndCenterDwi
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
import Teem (isNrrd, gzip)
import FSL (isNifti, tobvec, tobval)
import Shake.BuildNode

convertImage :: FilePath -> FilePath -> IO ()
convertImage infile outfile
  = if takeExtensions infile == takeExtensions outfile then
      IO.copyFile infile outfile
    else
      callProcess "ConvertBetweenFileFormats" [infile, outfile]

convertDwi :: FilePath -> FilePath -> Action ()
convertDwi infile outfile
  | takeExtensions infile == takeExtensions outfile = copyFile' infile outfile
  | isNrrd(infile) && isNrrd(outfile) = command_ [] "unu" ["save","-e","gzip","-f","nrrd","-i",infile,"-o",outfile]
  | isNrrd(infile) && isNifti(outfile) =
      command_ [] "DWIConvert" ["--conversionMode", "NrrdToFSL"
                               ,"--inputVolume", infile
                               ,"-o", outfile]
  | isNifti(infile) && isNrrd(outfile) = withTempDir $ \tmpdir -> do
      let dwiNiiShort = tmpdir </> "dwi-short.nii.gz"
          dwiNrrd = tmpdir </> "dwi.nrrd"
      command_ [] "ConvertBetweenFileFormats" [infile, dwiNiiShort ,"short"]
      command_ [] "DWIConvert" ["--conversionMode", "FSLToNrrd"
                               ,"--inputBVectors", tobvec $ infile
                               ,"--inputBValues", tobval $ infile
                               ,"--inputVolume", dwiNiiShort
                               ,"-o", dwiNrrd]
      command_ [] "unu" ["permute"
                        ,"-p", "1", "2", "3", "0"
                        ,"-i", dwiNrrd
                        ,"-o", dwiNrrd]
      liftIO $ Teem.gzip dwiNrrd
      copyFile' dwiNrrd outfile
  | otherwise = error $ "Dwi's must be nrrd or nifti: " ++ infile ++ ", " ++ outfile


maskImage :: FilePath -> FilePath -> FilePath -> IO ()
maskImage img mask out | FSL.isNifti out = maskImageUsing "nii.gz" FSL.mask out
                       | Teem.isNrrd out = maskImageUsing "nrrd" Teem.mask out
                       | otherwise = error "maskImage: images must be in Nrrd or Nifti format"
                       where
                         maskImageUsing ext maskFn out = withSystemTempDirectory "maskImage" $
                           \tmpdir -> do
                             let tmpmask = tmpdir </> "mask" <.> ext
                                 tmpimg = tmpdir </> "tmpimg" <.> ext
                             convertImage mask tmpmask
                             convertImage img tmpimg
                             maskFn tmpimg tmpmask out

extractB0 :: FilePath -> FilePath -> Action ()
extractB0 dwi out | FSL.isNifti dwi = withTempDir $ \tmpdir -> do
                      FSL.extractB0 dwi (tmpdir </> "b0.nii.gz")
                      liftIO $ Util.convertImage (tmpdir </> "b0.nii.gz") out
                  | Teem.isNrrd dwi = withTempDir $ \tmpdir -> do
                      liftIO $ Teem.extractB0 dwi (tmpdir </> "b0.nrrd")
                      liftIO $ Util.convertImage (tmpdir </> "b0.nrrd") out
                  | otherwise = error $ "extractB0: Invalid dwi, must be nrrd or nifti: " ++ dwi

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


alignAndCenter :: FilePath -> FilePath -> Action ()
alignAndCenter vol out = withTempDir $ \tmpdir -> do
    let nrrd = tmpdir </> "strct.nrrd"
    liftIO $ convertImage vol nrrd
    command_ [] "config/axis_align_nrrd.py" ["-i", nrrd, "-o", out]
    command_ [] "config/center.py" ["-i", out, "-o", out]

alignAndCenterDwi :: FilePath -> FilePath -> Action ()
alignAndCenterDwi vol out = withTempDir $ \tmpdir -> do
    let nrrd = tmpdir </> "dwiBeforeXc.nrrd"
    convertDwi vol nrrd
    command_ [] "config/axis_align_nrrd.py" ["-i", nrrd, "-o", out]
    command_ [] "config/center.py" ["-i", out, "-o", out]
