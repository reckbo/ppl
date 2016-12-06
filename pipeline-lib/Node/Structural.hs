{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
module Node.Structural
  ( StructuralType (..)
  , Structural (..)
  , rules
  ) where

import           Data.Maybe      (fromMaybe)
import           Node.Util
import           Shake.BuildNode

type CaseId = String

data StructuralType = T1w | T2w
                    deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

newtype Structural = Structural (StructuralType, CaseId)
                    deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

instance BuildNode Structural where
  path (Structural (T1w, caseid)) =  getPath "t1" caseid
  path (Structural (T2w, caseid)) =  getPath "t2" caseid


rules = rule (buildNode :: Structural -> Maybe (Action [Double]))
