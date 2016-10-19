{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import qualified HCP.Normalize
import           PNLPipeline
import           Development.Shake.Config

outdir = "_data"

---------------------------------------------------------------
-- main
main :: IO ()
main = shakeArgs shakeOptions{shakeFiles=outdir, shakeVerbosity=Chatty} $ do
  usingConfigFile "src/hcp.cfg"

  action $ do
    apply [HCP.Normalize.DwiPairsYaml "BIO_0001"] :: Action [Double]

  HCP.Normalize.rules
    -- action $ do
    --     let keys = [FaStats d m
    --                | d <- [DwiHarm, DwiEd]
    --                , m <- [FsMask, FsMaskNoCsf]
    --                ]
    --     apply keys :: Action [Double]

    -- rule (buildKey :: (DwiType, CaseId) -> Maybe (Action Double))
    -- rule (buildKey :: FaStats -> Maybe (Action Double))
