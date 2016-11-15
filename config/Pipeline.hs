{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
import           Data.List                (intercalate)
import           Development.Shake.Config
import           OutputDirectory          (outdir)
import           Shake.BuildKey
import           System.Directory         as IO
-- import           Software.UKFTractography (UKFTractographyExe (..), rules)
import           AntsPath                 (antsPath, antsSrc)
import qualified Intrust


type CaseId = String

data ImageType = Bse | Mask
        deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

data ImageFormat = Nrrd | Nifti
        deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

extFromFormat Nrrd = "nrrd"
extFromFormat Nifti = "nii.gz"

data BseAtlas = BseAtlas CaseId CaseId
        deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

instance BuildKey BseAtlas where
  paths x = [atlasPath Bse x, atlasPath Mask x]
    where
      atlasPath imgtype (BseAtlas caseid caseid')
        = outdir
        </> "atlases"
        </> (intercalate "-" [show imgtype, caseid, "in", caseid']) <.> "nii.gz"

  build out@(BseAtlas caseid caseidT) = Just $ withTempDir $ \tmpdir -> do
    let dwied = Intrust.path "dwied" caseid
        dwimask = Intrust.path "dwimask" caseid
        dwiedT = Intrust.path "dwied" caseidT
    need [dwied, dwiedT, dwimask, replaceExtension dwimask "raw.gz"]
    let bse = tmpdir </> "bse.nrrd"
        bseT = tmpdir </> "bseT.nrrd"
    unit $ cmd "bse.sh -i" dwied "-o" bse
    unit $ cmd "bse.sh -i" dwiedT "-o" bseT
    let pre = "bse_to_target"
        bseWarped = pre ++ "Warped.nii.gz"
    command_ [] (antsSrc </> "Scripts/antsRegistrationSyN.sh") ["-d", "3"
                                                              ,"-f", bseT
                                                              ,"-m", bse
                                                              ,"-o", pre
                                                              ,"-n", "16"]
    let xfmRigid = pre ++ "0GenericAffine.mat"
        xfmWarp = pre ++ "1Warp.nii.gz"
        xfm = pre ++ "Transform.nii.gz"
    command_ [] (antsPath </> "ComposeMultiTransform") ["3"
                                                      ,xfm
                                                      ,"-R", bseT
                                                      ,xfmWarp
                                                      ,xfmRigid]

    let [bseOut, maskOut] = paths out
    command_ [] (antsPath </> "antsApplyTransforms") ["-d", "3"
                                                    ,"-i", dwimask
                                                    ,"-o", maskOut
                                                    ,"-r", bseT
                                                    ,"-t", xfm]
    liftIO $ IO.renameFile bseWarped bseOut


main :: IO ()
main = shakeArgs shakeOptions{shakeFiles=outdir, shakeVerbosity=Chatty} $ do
  usingConfigFile "config/all.cfg"

  action $ do
    -- Just caseids <- fmap words <$> getConfig "caselist"
    let caseid = "003_GNX_007"
        caseidT = "016_NA3_015"
    apply [BseAtlas caseid caseidT] :: Action [[Double]]

  rule $ (buildKey :: BseAtlas -> Maybe (Action [Double]))
