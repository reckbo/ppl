import           Development.Shake.Config
import           Paths                    (outdir)
import           Pipeline
import           Shake.BuildNode

main :: IO ()
main = shakeArgs shakeOptions{shakeFiles=outdir, shakeVerbosity=Chatty} $ do
  usingConfigFile "config/settings.cfg"

  action $ do
    caseids <- readFileLines "config/caselist.txt"
    let nodes = [ FsInDwi (StructuralMaskMabs, DwiGiven, DwiMaskGiven, caseid)
                | caseid <- caseids]
    needs nodes

  rules
