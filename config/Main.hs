import           Development.Shake.Config
import           Paths                    (outdir)
import           Node
import           Shake.BuildNode

main :: IO ()
main = shakeArgs shakeOptions{shakeFiles=outdir
                             ,shakeVerbosity=Chatty
                             ,shakeTimings=True} $ do
  usingConfigFile "config/settings.cfg"

  action $ do
    caseids <- readFileLines "config/caselist.txt"
    let nodes = [ WmqlTracts (FreeSurferGiven, FsBrain_B0, DwiGiven, DwiMaskGiven, UKFTractographyDefault, caseid)
                | caseid <- caseids]
    needs nodes

  rules
