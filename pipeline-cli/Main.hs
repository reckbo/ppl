import           Control.Monad            (when)
import           Data.List                (intercalate)
import           Data.List.Split          (splitOn)
import           Data.Maybe               (fromJust)
-- import           Data.Yaml
import           Development.Shake.Config
import           Need
import           Node
import           Paths
import           Shake.BuildNode
import           System.Directory         (getCurrentDirectory)
import           Types


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
     ["caselist=" ++ projdir ++ "/_config/caselist.txt"] ++
     map (\(k,v) -> k ++ "=" ++ escape v) ps

inputs :: Inputs
inputs =
  Inputs {fstypes =
            [FreeSurferUsingMask (T1wGiven "t1")
                                 (NormalMask (StructuralMaskMabs bthash))
            |bthash <- ["asdfeasf","ddddd1"]] ++
            [FreeSurferGiven "fs"]
         ,dwimaskpairs =
            [(DwiXC (DwiGiven key),DwiMaskHcp)|key <- ["hcp","buddi"]] ++
            [(DwiXC (DwiEpi (DwiGiven "dwi")
                            DwiMaskHcp
                            (T2wGiven "t2")
                            (RigidMask bthash
                                       (StructuralMaskMabs bthash)
                                       (T1wXc "t1"))
                            bthash)
             ,DwiMaskEpi)|bthash <- ["asdf"]]
         ,ukftypes = [UKFTractographyDefault]
         ,fs2dwimethods = [FsBrain_B0]}

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
                    -- liftIO $ encodeFile "output.yml" inputs
                    -- input <- liftIO $ fmap fromJust . decodeFile $ "output.yml"
                    -- liftIO $ print (input :: Inputs)
            else let tractMeasureNodes =
                       concatMap tractMeasuresFromCaseid caseids
                     fsInDwiNodes = concatMap fsInDwiFromCaseid caseids
                     ukfNodes = concatMap ukfFromCaseid caseids
                     wmqlNodes = concatMap wmqlFromCaseid caseids
                     dwiNodes = concatMap dwiFromCaseid caseids
                 in do when (not $ null tractMeasureNodes)
                            (do needs (tractMeasureNodes :: [TractMeasures])
                                return ())
                       when (not $ null fsInDwiNodes)
                            (do needs (fsInDwiNodes :: [FsInDwi])
                                return ())
                       when (not $ null dwiNodes)
                            (do needs (dwiNodes :: [Dwi])
                                return ())
       rules
