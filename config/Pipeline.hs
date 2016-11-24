import           Development.Shake.Config
import           BuildNode.HCP                      (HcpDwi (..), rules)
import           PathsOutputRoot          (outdir)
import           Shake.BuildNode
import           BuildNode.UKFTractography (UKFTractographyExe (..), rules)
import           qualified BuildNode.TractQuerier (TractQuerier (..), rules)
import           qualified BuildNode.ANTs (ANTs (..), rules)
import qualified BuildNode.MABS (Mask (..), rules)
import qualified BuildNode.FreeSurfer (FreeSurfer (..), rules)


main :: IO ()
main = shakeArgs shakeOptions{shakeFiles=outdir, shakeVerbosity=Chatty} $ do
  usingConfigFile "config/settings.cfg"

  action $ do
    Just caseids <- fmap words <$> getConfig "caselist"
    let nodes = [BuildNode.FreeSurfer.FreeSurfer caseid | caseid <- caseids]
    apply nodes :: Action [[Double]]

  BuildNode.FreeSurfer.rules
  BuildNode.MABS.rules
  BuildNode.UKFTractography.rules
  BuildNode.TractQuerier.rules
  BuildNode.ANTs.rules
  BuildNode.HCP.rules
