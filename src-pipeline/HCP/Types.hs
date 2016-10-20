{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module HCP.Types
  ( PhaseDirection (..)
  , Direction (..)
  , CaseId
  , DwiType (..)
  , EchoSpacing
  , PhaseLength
  ) where

import           Shake.BuildKey

type CaseId = String
type EchoSpacing = Float
type PhaseLength = Int

data PhaseDirection = RL | LR | PA | AP
        deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

data Direction = Pos | Neg
        deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

data DwiType = NormalizedDwi
             | SourceDwi
        deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)
