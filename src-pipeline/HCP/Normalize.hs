{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module HCP.Normalize
  (DwiPairsYaml (..)
  ,meanb0_file
  ,rules
  )
  where

import           Data.List
import           Data.List.Split            (splitOn)
import           Data.Yaml                  (encodeFile)
import           Development.Shake
import           Development.Shake.Config
import           Development.Shake.FilePath
import           FSL                        (takeBaseName', tobval, tobvec)
import           HCP.Types                  (DWIInfo (..), DWIPair (..))
import           HCP.Util                   (getB0sMean, readDWIPair, scaleDWI)
import           Text.Printf
import           PNLPipeline

outdir :: [Char]
outdir = "_data"

meanb0_file :: FilePath
meanb0_file = outdir </> "Pos-1-meanb0"

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

    [outdir </> "*.nii.gz",
     outdir </> "*.bval",
     outdir </> "*.bvec"]
      *>> \[dwiOut, bvalOut, bvecOut] ->
        let
          [dirtype, pid] = splitOn "-" $ takeBaseName' dwiOut
          process key pid = do
            Just dwis <- fmap words <$> getConfig key
            let dwiSrc = dwis !! (pid-1)
            need [dwiSrc, tobval dwiSrc, tobvec dwiSrc, meanb0_file]
            mean0 <- read <$> readFile' meanb0_file
            scaleDWI dwiOut dwiSrc (tobval dwiSrc) mean0
            copyFile' (tobval dwiSrc) bvalOut
            copyFile' (tobvec dwiSrc) bvecOut
        in
          case dirtype of
            "Pos" -> process "posdwis" (read pid)
            "Neg" -> process "negdwis" (read pid)
            _ -> error "This rule builds dwi's with format e.g. Pos-1.nii.gz"

    meanb0_file
      %> \_ -> do
        posdwi0 <- fmap head <$> getConfigWithCaseId "posdwis" "BIO_0001"
        need [posdwi0, tobval posdwi0]
        mean0 <- getB0sMean posdwi0 (tobval posdwi0)
        writeFile' meanb0_file $ show mean0
