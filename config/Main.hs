import           Development.Shake.Config
import           Node
import           Node.MeasureTractsCsv    hiding (rules)
import           Paths                    (outdir)
import           Shake.BuildNode

main :: IO ()
main = shakeArgs shakeOptions{shakeFiles=outdir
                             ,shakeVerbosity=Chatty
                             ,shakeTimings=True} $ do
  usingConfigFile "config/settings.cfg"

  action $ do
    caseids <- readFileLines "config/caselist.txt"
    let nodes = [
          MeasureTractsCsv {fstype=FreeSurferGiven
                           ,fs2dwitype=FsBrain_B0
                           ,dwitype=DwiGiven
                           ,dwimasktype=DwiMaskGiven
                           ,ukftype=UKFTractographyDefault
                           ,caseid=caseid}
                | caseid <- caseids]
    needs nodes

  rules
