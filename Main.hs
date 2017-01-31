import           Control.Monad            (when)
import           Data.List                (intercalate)
import           Data.List.Split          (splitOn)
import           Development.Shake.Config
import           Need
import           Node
import           Node.DWI                 hiding (rules)
import           Node.DWIMask             hiding (rules)
import           Paths
import           Shake.BuildNode
import           System.Directory         (getCurrentDirectory)


makeSetUpData :: FilePath -> String
makeSetUpData projdir =
  let rplc before after s = intercalate after . splitOn before $ s
      escape = rplc ")" "\\)" . rplc "(" "\\("
      ps =
        concat $
        zipWith (pathsMeasureTracts projdir)
                [1 ..]
                (measureTractsFromCaseid "$case")
      ps' =
        concat $
        zipWith (pathsWmql projdir)
                [1 ..]
                (wmqlFromCaseid "$case")
  in unlines $
     ["caselist=" ++ projdir ++ "/config/caselist.txt"] ++
     map (\(k,v) -> k ++ "=" ++ escape v) ps

main :: IO ()
main =
  shakeArgsWith
    shakeOptions {shakeFiles = Paths.outdir
                 ,shakeVerbosity = Chatty
                 ,shakeReport = ["report.html","report.json"]}
    [] $
  \_ caseids ->
    return $
    Just $
    do usingConfigFile "config/settings.cfg"
       action $
         if null caseids
            then do projdir <- liftIO $ getCurrentDirectory
                    writeFile' (Paths.outdir </> "SetUpData.sh") $
                      makeSetUpData projdir
                    liftIO $
                      print $ map path $ measureTractsFromCaseid "$[case]"
            else let measureTractNodes =
                       concatMap measureTractsFromCaseid caseids
                     wmparcInDwiNodes = concatMap wmparcInDwiFromCaseid caseids
                     ukfNodes = concatMap ukfFromCaseid caseids
                     wmqlNodes = concatMap wmqlFromCaseid caseids
                 in do when (not $ null measureTractNodes)
                            (do needs (measureTractNodes :: [MeasureTractsCsv])
                                return ())
                       when (not $ null wmparcInDwiNodes)
                            (do needs (wmparcInDwiNodes :: [WmparcInDwi])
                                return ())
       rules
