import           Development.Shake.Config
import           BuildNode.HCP                      (HcpDwi (..), rules)
import           PathsOutputRoot          (outdir)
import           Shake.BuildNode
import           BuildNode.UKFTractography (UKFTractographyExe (..), rules)
import           qualified BuildNode.TractQuerier (TractQuerier (..), rules)
import           qualified BuildNode.ANTs (ANTs (..), rules)
import qualified BuildNode.MABS (Mask (..), rules)


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
    apply1 (BuildNode.MABS.Mask "HumanTest") :: Action [Double]

  BuildNode.MABS.rules
  BuildNode.UKFTractography.rules
  BuildNode.TractQuerier.rules
  BuildNode.ANTs.rules
  BuildNode.HCP.rules
