import           Development.Shake.Config
import           Node
import           Node.MeasureTractsCsv    hiding (rules)
import           Paths                    (outdir)
import           Shake.BuildNode
import           System.Console.GetOpt
-- import Data.Maybe ( fromMaybe )


-- options :: [OptDescr String-- ]
-- options = [
--      Option ['l']  ["caselist"]  (OptArg (fromMaybe "stdin") "FILE")  "output FILE"
--      ]

main :: IO ()
-- main = shakeArgs shakeOptions{shakeFiles=outdir ,shakeVerbosity=Chatty} $ do
main = shakeArgsWith shakeOptions{shakeFiles=outdir,shakeVerbosity=Chatty} [] $ \_ targets -> return $ Just $ do
  usingConfigFile "config/settings.cfg"

  action $ do
    caseids <- if null targets
               then readFileLines "config/caselist.txt"
               else return targets
    let nodes = [
          MeasureTractsCsv {fstype=FreeSurferWithMask StructuralMaskMabs
                           ,fs2dwitype=FsBrain_B0
                           ,dwitype=DwiGiven
                           ,dwimasktype=DwiMaskGiven
                           ,ukftype=UKFTractographyDefault
                           ,caseid=caseid}
                | caseid <- caseids]
    needs nodes

  rules
