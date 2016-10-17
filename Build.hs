{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import qualified Nrrd        (fa, makeMask, mask)
import           PNLPipeline

outdir = "_data"

---------------------------------------------------------------
-- main
main :: IO ()
main = shakeArgs shakeOptions{shakeFiles=outdir, shakeVerbosity=Chatty} $ do
  return ()
    -- action $ do
    --     let keys = [FaStats d m
    --                | d <- [DwiHarm, DwiEd]
    --                , m <- [FsMask, FsMaskNoCsf]
    --                ]
    --     apply keys :: Action [Double]

    -- rule (buildKey :: (DwiType, CaseId) -> Maybe (Action Double))
    -- rule (buildKey :: FaStats -> Maybe (Action Double))