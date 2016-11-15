import           Development.Shake.Config
import           HCP                      (rules)
import           HCP.PostEddy
import           OutputDirectory          (outdir)
import           Shake.BuildKey
import           Software.UKFTractography (UKFTractographyExe (..), rules)
import           qualified Software.TractQuerier (TractQuerier (..), rules)


main :: IO ()
main = shakeArgs shakeOptions{shakeFiles=outdir, shakeVerbosity=Chatty} $ do
  usingConfigFile "config/all.cfg"

  action $ do
    -- Just caseids <- fmap words <$> getConfig "caselist"
    -- apply [HcpDwi caseid | caseid <- caseids] :: Action [[Double]]

    -- Just ukf_hash <- getConfig "UKFTractography_hash"
    -- apply1 (UKFTractographyExe ukf_hash) :: Action [Double]

    apply1 (Software.TractQuerier.TractQuerier "a8e354e") :: Action String

  HCP.rules
  Software.UKFTractography.rules
  Software.TractQuerier.rules
