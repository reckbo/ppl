import           Control.Monad            (when)
import           Data.List                (intercalate)
import           Data.List.Split          (splitOn)
import           Development.Shake.Config
import           Need
import           Node
import           Paths
import           Shake.BuildNode
import           System.Directory         (getCurrentDirectory)


makeSetUpData :: FilePath -> String
makeSetUpData projdir =
  let rplc before after s = intercalate after . splitOn before $ s
      escape = rplc ")" "\\)" . rplc "(" "\\("
      ps =
        concat $
        zipWith (pathsTractMeasures projdir)
                [1 ..]
                (tractMeasuresFromCaseid "$case")
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
                 ,shakeReport = map (combine Paths.outdir) ["report.html","report.json"]}
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
            else let tractMeasureNodes =
                       concatMap tractMeasuresFromCaseid caseids
                     fsInDwiNodes = concatMap fsInDwiFromCaseid caseids
                     ukfNodes = concatMap ukfFromCaseid caseids
                     wmqlNodes = concatMap wmqlFromCaseid caseids
                 in do when (not $ null tractMeasureNodes)
                            (do needs (tractMeasureNodes :: [TractMeasures])
                                return ())
                       when (not $ null fsInDwiNodes)
                            (do needs (fsInDwiNodes :: [FsInDwi])
                                return ())
       rules
