import           Development.Shake.Config
import           Node
import           Paths
import           Shake.BuildNode
import           Need
import Node.DWI hiding (rules)
import Node.DWIMask hiding (rules)
import           Data.List       (intercalate)
import           Data.List.Split (splitOn)

dwis subjid = [Dwi (dwiType, subjid)
             | dwiType <- dwiTypes]

dwimasks subjid = [DwiMask (dwimaskType, dwiType, subjid)
                 | dwimaskType <- dwimaskTypes
                 , dwiType <- dwiTypes]

ukfs subjid = [UKFTractography (ukfType, dwiType, dwimaskType, subjid)
             | ukfType <- ukfTypes
             , dwiType <- dwiTypes
             , dwimaskType <- dwimaskTypes]

fs subjid = [FreeSurfer (fsType, subjid)
            | fsType <- fsTypes]

fsindwis subjid =  [ WmparcInDwi (fs2dwiType, fsType, dwiType, dwimaskType, subjid)
                  | fs2dwiType <- fs2dwiTypes
                  , fsType <- fsTypes
                  , dwiType <- dwiTypes
                  , dwimaskType <- dwimaskTypes]

wmqls subjid = [ WmqlTracts fsType fs2dwiType dwiType dwimaskType ukfType subjid
              | fs2dwiType <- fs2dwiTypes
              , fsType <- fsTypes
              , dwiType <- dwiTypes
              , dwimaskType <- dwimaskTypes
              , ukfType <- ukfTypes]

measuretracts subjid = [ MeasureTractsCsv fsType fs2dwiType dwiType dwimaskType ukfType subjid
                       | fs2dwiType <- fs2dwiTypes
                       , fsType <- fsTypes
                       , dwiType <- dwiTypes
                       , dwimaskType <- dwimaskTypes
                       , ukfType <- ukfTypes]

setUpData :: String
setUpData = unlines s
        where
          mkvar key nodes = zipWith3 (\k i p -> concat [k, show i, "=", p]) (repeat key) [1..] paths
                          where paths = map (escape . path) (nodes "${case}")
          rplc before after s = intercalate after . splitOn before $ s
          escape = rplc ")" "\\)" . rplc "(" "\\("
          s = mkvar "dwi" dwis ++
            mkvar "dwimask" dwimasks ++
            mkvar "ukf" ukfs ++
            mkvar "fs" fs ++
            mkvar "fsindwi" fsindwis ++
            mkvar "wmql" wmqls ++
            mkvar "measuretracts" measuretracts ++
            ["caselist=../config/caselist.txt"] ++
            ["status_vars='dwi dwimask fs fsindwi wmql measuretracts'"]

main :: IO ()
main = shakeArgsWith shakeOptions{shakeFiles=Paths.outdir
                                 ,shakeVerbosity=Chatty
                                 ,shakeReport=["report.html","report.json"]} []
                                 $ \_ caseids -> return $ Just $ do
  usingConfigFile "config/settings.cfg"

  action $ do
    if null caseids
      then writeFile' (Paths.outdir </> "SetUpData.sh") setUpData
      else do _ <- needs $ concatMap measuretracts caseids
              return ()

  rules
