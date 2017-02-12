{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
module T1wMask where

import           Shake.BuildNode
import           T1w
import           Types
import           Software.BrainsTools

data T1wMask =
  T1wMask {t1masktype :: T1wMaskType
          ,t1type :: T1wType
          ,caseid :: CaseId}

instance Show T1wMask
instance Generic T1wMask
instance Typeable T1wMask
instance Eq T1wMask
instance Hashable T1wMask
instance Binary T1wMask
instance NFData T1wMask
instance Read T1wMask
instance BuildNode T1wMask