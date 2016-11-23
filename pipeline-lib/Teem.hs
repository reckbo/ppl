module Teem
  (mask
  ,fa
  ,gzip
  ,makeMask
  ,isNrrd
  ,center
  )
  where

import           Development.Shake
import           Development.Shake.Command
import           Development.Shake.FilePath
import           System.Process             (callProcess)

gzip :: FilePath -> IO ()
gzip out = callProcess "unu" ["save","-e","gzip","-f","nrrd","-i",out,"-o",out]

mask :: FilePath -> FilePath -> FilePath -> IO ()
mask mask vol out = do
  callProcess "unu" ["3op", "ifelse", mask, vol
                    , "0", "-o", out]
  gzip out

fa :: FilePath -> FilePath -> Action ()
fa dwi out = do
  withTempFile $ \tensor -> do
    command_ [] "tend" ["estim","-est","lls","-B","kvp","-knownB0","true","-i",dwi,"-o",tensor]
    command_ [] "tend" ["anvol","-t","-1","-a","fa","-i",tensor,"-o",out]
  liftIO $ gzip out

makeMask :: FilePath -> FilePath -> Action()
makeMask invol outvol = do
      command_ [] "unu" ["3op","ifelse",invol,"1","0","-o",outvol]
      liftIO $ gzip outvol

isNrrd :: FilePath -> Bool
isNrrd file = ext == "nrrd)" || ext == "nhdr"
  where
    ext = takeExtension file

toNifti :: FilePath -> FilePath -> Action ()
toNifti nrrd out = unit $ cmd "ConvertBetweenFileFormats" nrrd out

center :: FilePath -> IO ()
center nrrd = callProcess "center.py"
  ["-i", nrrd
  ,"-o", nrrd]
