import           Development.Shake.Config
import           Node
import           Paths
import           Shake.BuildNode
import           Need
import Node.DWI hiding (rules)
import Node.DWIMask hiding (rules)
import           Data.List       (intercalate)
import           Data.List.Split (splitOn)

dwi subjid = [Dwi (dwiType, subjid)
             | dwiType <- dwiTypes]

dwimask subjid = [DwiMask (dwimaskType, subjid)
                 | dwimaskType <- dwimaskTypes]

fs subjid = [FreeSurfer (fsType, subjid)
            | fsType <- fsTypes]

fsindwi subjid =  [ WmparcInDwi (fs2dwiType, fsType, dwiType, dwimaskType, subjid)
                  | fs2dwiType <- fs2dwiTypes
                  , fsType <- fsTypes
                  , dwiType <- dwiTypes
                  , dwimaskType <- dwimaskTypes]

wmql subjid = [ WmqlTracts fsType fs2dwiType dwiType dwimaskType ukfType subjid
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
  where mkvar key path' = key ++ "=" ++ path'
        rplc before after s = intercalate after . splitOn before $ s
        escape = rplc ")" "\\)" . rplc "(" "\\("
        s = map (mkvar "measuretracts" . escape . path) (measuretracts "${case}") ++
            map (mkvar "dwi" . escape . path) (dwi "${case}") ++
            map (mkvar "dwimask" . escape . path) (dwimask "${case}") ++
            map (mkvar "wmql" . escape . path) (wmql "${case}") ++
            map (mkvar "fs" . escape . path) (fs "${case}") ++
            map (mkvar "fsindwi" . escape . path) (fsindwi "${case}") ++
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
