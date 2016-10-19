{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module HCP.Normalize
  (DwiPairsYaml (..)
  ,MeanB0 (..)
  ,rules
  )
  where

import           Data.List
import           Data.List.Split            (splitOn)
import           Data.Yaml                  (encodeFile)
import           Development.Shake
import           Development.Shake.Config
import           Development.Shake.FilePath
import           FSL                        (BValue, extractVols_, readbval,
                                             takeBaseName', tobval, tobvec)
import           HCP.Config
import           HCP.DWIPair                (DWIInfo (..), DWIPair (..),
                                            readDWIPair)
import           PNLPipeline
import           Text.Printf

outdir :: [Char]
outdir = "_data"

getB0sMean :: FilePath -> FilePath -> Action Float
getB0sMean dwi bval = do
  b0indices <- findIndices (< b0maxbval) <$> readbval bval
  withTempFile $ \b0s -> do
    extractVols_ b0s dwi b0indices
    command_ [] "fslmaths" [b0s, "-Tmean", b0s]
    Stdout mean <- command [] "fslmeants" ["-i", b0s]
    return $ read mean

scaleDWI :: FilePath -> FilePath -> FilePath -> Float -> Action ()
scaleDWI out src srcBval mean0 = do
  mean <- getB0sMean src srcBval
  command_ [] "fslmaths" [src
                         ,"-mul", show mean0
                         ,"-div", show mean
                         ,out]

newtype MeanB0 = MeanB0 CaseId
        deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData)

instance BuildKey MeanB0 where
  path (MeanB0 caseid) =
    outdir </> caseid </> "hcp" </> "0_normalized" </> "Pos-1-meanb0"
  build n@(MeanB0 caseid) = Just $ do
        posdwi0 <- head <$> getConfigWithCaseId "posdwis" caseid
        need [posdwi0, tobval posdwi0]
        mean0 <- getB0sMean posdwi0 (tobval posdwi0)
        writeFile' (path n) $ show mean0

newtype DwiPairsYaml = DwiPairsYaml CaseId
        deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData)

instance BuildKey DwiPairsYaml where
  path (DwiPairsYaml caseid) =
    outdir </> caseid </> "hcp" </> "0_normalized" </> "dwipairs.yaml"
  build n@(DwiPairsYaml caseid) = Just $ do
    posdwis <- getConfigWithCaseId "posdwis" caseid
    negdwis <- getConfigWithCaseId "negdwis" caseid
    dwipairs <- traverse readDWIPair $ zip3 [1..] posdwis negdwis
    let updatePath dwiinfo@DWIInfo{_pid=pid,_dirType=dirType}
          = dwiinfo {_dwi=dwinew}
          where
            dwinew =
              printf (takeDirectory (path n) </> "%s-%i.nii.gz") (show dirType) pid
        posNew = map (updatePath._pos) dwipairs
        negNew = map (updatePath._neg) dwipairs
    liftIO $ encodeFile (path n) $ zipWith DWIPair posNew negNew


rules :: Rules ()
rules = do

    rule (buildKey :: DwiPairsYaml -> Maybe (Action Double))
    rule (buildKey :: MeanB0 -> Maybe (Action Double))

    -- [outdir </> "*.nii.gz",
    --  outdir </> "*.bval",
    --  outdir </> "*.bvec"]
    --   *>> \[dwiOut, bvalOut, bvecOut] ->
    --     let
    --       [dirtype, pid] = splitOn "-" $ takeBaseName' dwiOut
    --       process key pid = do
    --         Just dwis <- fmap words <$> getConfig key
    --         let dwiSrc = dwis !! (pid-1)
    --         need [dwiSrc, tobval dwiSrc, tobvec dwiSrc, meanb0_file]
    --         mean0 <- read <$> readFile' meanb0_file
    --         scaleDWI dwiOut dwiSrc (tobval dwiSrc) mean0
    --         copyFile' (tobval dwiSrc) bvalOut
    --         copyFile' (tobvec dwiSrc) bvecOut
    --     in
    --       case dirtype of
    --         "Pos" -> process "posdwis" (read pid)
    --         "Neg" -> process "negdwis" (read pid)
    --         _ -> error "This rule builds dwi's with format e.g. Pos-1.nii.gz"
