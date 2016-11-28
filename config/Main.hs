import           Development.Shake.Config
import           Paths                    (outdir)
import           Pipeline
import           Shake.BuildNode

main :: IO ()
main = shakeArgs shakeOptions{shakeFiles=outdir, shakeVerbosity=Chatty} $ do
  usingConfigFile "config/settings.cfg"

  action $ do
    Just caseids <- fmap words <$> getConfig "caselist"
    let nodes = [(FsInDwi, StructuralMaskSource, DwiSource, DwiMaskSource, caseid)
                | caseid <- caseids]
    needs nodes

  rules
