module SoftwareOutputPaths
  (ukfTractographyPrefix
  ,tractQuerierPrefix
  ,antsPrefix
  )
  where

import           Development.Shake.FilePath (FilePath, (</>))
import           OutputDirectory            (outdir)

ukfTractographyPrefix = outdir </> "software" </> "UKFTractography"

tractQuerierPrefix = outdir </> "tract_querier"

antsPrefix = outdir </> "ANTs"