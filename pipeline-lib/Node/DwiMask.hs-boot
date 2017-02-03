module Node.DwiMask
  ( DwiMask (..)
  ) where

import           Node.Types
import           Shake.BuildNode

data DwiMask =
  DwiMask {dwimaskmethod :: DwiMaskMethod
          ,dwitype :: DwiType
          ,caseid :: CaseId}

instance Show DwiMask
instance Generic DwiMask
instance Typeable DwiMask
instance Eq DwiMask
instance Hashable DwiMask
instance Binary DwiMask
instance NFData DwiMask
instance Read DwiMask
instance BuildNode DwiMask