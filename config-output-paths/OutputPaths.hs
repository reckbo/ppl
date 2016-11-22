module OutputPaths
  (t1MaskMabs
  ,ukfTractographyPrefix
  ,tractQuerierPrefix
  ,antsPrefix
  )
  where

import           Development.Shake.FilePath (FilePath, (</>))
import           OutputDirectory            (outdir)

--------------------------------------------------------------------------------
-- Data

t1MaskMabs caseid = outdir </> caseid </> caseid ++ "-" ++ "mabsT1Mask.nii.gz"

--------------------------------------------------------------------------------
-- Software

ukfTractographyPrefix = outdir </> "software" </> "UKFTractography"
tractQuerierPrefix = outdir </> "tract_querier"
antsPrefix = outdir </> "ANTs"