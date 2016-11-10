module SoftwareOutputPaths
  (ukfTractographyExe)
  where

import           Development.Shake.FilePath (FilePath, (</>))
import           OutputDirectory            (outdir)

ukfTractographyExe :: String -> FilePath
ukfTractographyExe hash = outdir </> "software" </> ("UKFTractography-" ++ hash)
