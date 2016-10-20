{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import           Development.Shake.Config
import HCP.Normalize (DwiPairsYaml (..), HcpDwi (..), MeanB0 (..),rules)
import           HCP.Types
import           Shake.BuildKey
import HCP.Config (outdir)
-- import qualified HCP.Preprocessing


main :: IO ()
main = shakeArgs shakeOptions{shakeFiles=outdir, shakeVerbosity=Chatty} $ do
  usingConfigFile "src/hcp.cfg"

  action $ do
    Just caseids <- fmap words <$> getConfig "caselist"
    apply $ map DwiPairsYaml caseids :: Action [[Double]]
    let keys = do
          dir <- [Pos, Neg]
          num <- [1,2]
          caseid <- ["BIO_0001"]
          return $ HcpDwi NormalizedDwi dir num caseid
    apply keys :: Action [[Double]]


  -- want [HCP.Preprocessing.posbval]

  HCP.Normalize.rules
  -- HCP.Preprocessing.rules
    -- action $ do
    --     let keys = [FaStats d m
    --                | d <- [DwiHarm, DwiEd]
    --                , m <- [FsMask, FsMaskNoCsf]
    --                ]
    --     apply keys :: Action [Double]

    -- rule (buildKey :: (DwiType, CaseId) -> Maybe (Action Double))
    -- rule (buildKey :: FaStats -> Maybe (Action Double))
