import           Development.Shake.Config
import           Node
import           Paths
import           Shake.BuildNode
import           Need

main :: IO ()
main = shakeArgsWith shakeOptions{shakeVerbosity=Chatty
                                 ,shakeReport=["report.html","report.json"]} [] 
                                 $ \_ targets -> return $ Just $ do
  usingConfigFile "config/settings.cfg"

  action $ do
    caseids <- if null targets
               then readFileLines "config/caselist.txt"
               else return targets
    Need.need caseids

  rules
