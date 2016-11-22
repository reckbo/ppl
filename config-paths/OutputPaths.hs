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
freesurfer caseid = outdir </> caseid </> caseid ++ "-" ++ "freesurfer"

--------------------------------------------------------------------------------
-- Software

ukfTractographyPrefix = outdir </> "software" </> "UKFTractography"
tractQuerierPrefix = outdir </> "tract_querier"
antsPrefix = outdir </> "ANTs"