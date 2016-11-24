module PathsOutput
  (t1MaskMabs
  ,freeSurfer
  -- software
  ,ukfTractographyPrefix
  ,tractQuerierPrefix
  ,antsPrefix
  )
  where

import           Development.Shake.FilePath (FilePath, (</>))
import           PathsOutputRoot            (outdir)

--------------------------------------------------------------------------------
-- Data

t1MaskMabs caseid = outdir </> caseid </> caseid ++ "-" ++ "mabsT1Mask.nii.gz"
freeSurfer caseid = outdir </> caseid </> caseid ++ "-" ++ "freesurfer"

--------------------------------------------------------------------------------
-- Software

ukfTractographyPrefix = outdir </> "UKFTractography"
tractQuerierPrefix = outdir </> "tract_querier"
antsPrefix = outdir </> "ANTs"
