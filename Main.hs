import           Development.Shake.Config
import           Node
import           Paths
import           Shake.BuildNode
import           Need

main :: IO ()
main = shakeArgsWith shakeOptions{shakeVerbosity=Chatty} [] $ \_ targets -> return $ Just $ do
  usingConfigFile "config/settings.cfg"

  action $ do
    caseids <- if null targets
               then readFileLines "config/caselist.txt"
               else return targets
    Need.need caseids

  -- (outdir </> "tractmeasures.csv") %> \out -> do
  --   caseids <- if null targets
  --              then readFileLines "config/caselist.txt"
  --              else return targets
  --   let csvs = [caseid </> "MeasureTractsCsv*.csv" | caseid <- caseids]
  --   Stdout stdout <- cmd "csvstack" csvs
  --   liftIO $ writeFile out stdout

  rules
