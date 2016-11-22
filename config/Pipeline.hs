import           Development.Shake.Config
import           HCP                      (rules)
import           HCP.PostEddy
import           PathsOutputRoot          (outdir)
import           Shake.BuildNode
import           Software.UKFTractography (UKFTractographyExe (..), rules)
import           qualified Software.TractQuerier (TractQuerier (..), rules)
import           qualified Software.ANTs (ANTs (..), rules)
import qualified MABS (Mask (..), rules)


main :: IO ()
main = shakeArgs shakeOptions{shakeFiles=outdir, shakeVerbosity=Chatty} $ do
  usingConfigFile "config/settings.cfg"

  action $ do
    -- Just caseids <- fmap words <$> getConfig "caselist"
    -- apply [HcpDwi caseid | caseid <- caseids] :: Action [[Double]]

    -- Just ukf_hash <- getConfig "UKFTractography_hash"
    -- apply1 (UKFTractographyExe ukf_hash) :: Action [Double]

    -- apply1 (Software.TractQuerier.TractQuerier "a8e354e") :: Action String
    Just antshash <- getConfig "ANTs-hash"
    apply1 (MABS.Mask "HumanTest_2016007") :: Action [Double]

  MABS.rules
  Software.UKFTractography.rules
  Software.TractQuerier.rules
  Software.ANTs.rules
  HCP.rules
