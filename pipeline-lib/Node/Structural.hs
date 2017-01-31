{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
module Node.Structural
  (Structural(..)
  ,rules)
  where

import           Data.Maybe      (fromMaybe)
import           Node.Types
import           Node.Util
import           Shake.BuildNode
import           Util            (alignAndCenter)


newtype Structural = Structural (StructuralType, CaseId)
                    deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

instance BuildNode Structural where
  path (Structural (T1w, caseid)) =  getPath "t1" caseid
  path (Structural (T2w, caseid)) =  getPath "t2" caseid
  path n@(Structural (StructuralXC _, caseid)) = outdir </> caseid </> showKey n <.> "nrrd"

  build out@(Structural (StructuralXC strcttype, caseid)) = Just $ do
    let strctNode = Structural (strcttype, caseid)
    need strctNode
    alignAndCenter (path strctNode) (path out)

rules = rule (buildNode :: Structural -> Maybe (Action [Double]))
