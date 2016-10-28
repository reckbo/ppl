{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import           Development.Shake.Config
import           HCP.Config               (outdir)
import           HCP.Normalize            (B0sPairsYaml (..), DwiScan (..),
                                           MeanB0 (..), rules)
import           HCP.Topup1
import           HCP.Types
import           HCP (rules)
import           Shake.BuildKey


main :: IO ()
main = shakeArgs shakeOptions{shakeFiles=outdir, shakeVerbosity=Chatty} $ do
  usingConfigFile "src/hcp.cfg"

  action $ do
    Just caseids <- fmap words <$> getConfig "caselist"
    -- apply $ map B0sPairsYaml caseids :: Action [[Double]]
    -- apply $ map AcqParams caseids :: Action [[Double]]
    -- apply $ map Index caseids :: Action [[Double]]
    -- apply $ (do
    --   orient <- [Pos,Neg]
    --   caseid <- caseids
    --   return $ Series orient caseid ) :: Action [[Double]]
    -- apply $ (do
    --   orient <- [Pos,Neg]
    --   caseid <- caseids
    --   return $ B0s orient caseid ) :: Action [[Double]]
    apply [TopupOutput caseid | caseid <- caseids] :: Action [[Double]]

  HCP.rules
