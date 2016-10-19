{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module HCP.DwiTypes
  ( PhaseDirection (..)
  , Direction (..)
  , DwiType (..)
  , HcpDwi (..)
  ) where

import           PNLPipeline

data PhaseDirection = RL | PA
        deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

data Direction = Pos | Neg
        deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

data DwiType = NormalizedDwi
             | SourceDwi
        deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

data HcpDwi = HcpDwi DwiType Direction Int CaseId
        deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)
