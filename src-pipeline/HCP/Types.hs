{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module HCP.Types
  ( PhaseDirection (..)
  , Direction (..)
  , DwiType (..)
  , HcpDwi (..)
  , CaseId
  ) where

import           Shake.BuildKey

type CaseId = String

data PhaseDirection = RL | PA
        deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

data Direction = Pos | Neg
        deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

data DwiType = NormalizedDwi
             | SourceDwi
        deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

data HcpDwi = HcpDwi DwiType Direction Int CaseId
        deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)
