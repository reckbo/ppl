{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import           Development.Shake.Config
import           HCPConfig               (outdir)
import           HCP (rules)
import           HCP.PostEddy
import           Shake.BuildKey


main :: IO ()
main = shakeArgs shakeOptions{shakeFiles=outdir, shakeVerbosity=Chatty} $ do
  usingConfigFile "src/hcp/hcp.cfg"

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
    -- apply [HiFiB0 caseid | caseid <- caseids] :: Action [[Double]]
    -- apply [EddyUnwarpedImages caseid | caseid <- caseids] :: Action [[Double]]
    apply [DataDwi caseid | caseid <- caseids] :: Action [[Double]]

  HCP.rules
