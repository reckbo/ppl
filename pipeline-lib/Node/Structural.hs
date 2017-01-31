{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}
module Node.Structural
  (Structural(..)
  ,rules)
  where

import           Node.Types
import           Node.Util       (getPath, outdir, showKey)
import           Shake.BuildNode
import           Util            (alignAndCenter)


data Structural =
  Structural {strcttype :: StructuralType
             ,caseid    :: CaseId}
  deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

instance BuildNode Structural where
  path (Structural T1w caseid) = getPath "t1" caseid
  path (Structural T2w caseid) = getPath "t2" caseid
  path n@(Structural (StructuralXC _) caseid) =
    outdir </> caseid </> showKey n <.> "nrrd"
  build out@(Structural{..}) =
    case strcttype of
      (StructuralXC strcttype) ->
        Just $
        do need Structural {..}
           alignAndCenter (path Structural {..})
                          (path out)
      _ -> Nothing

rules = rule (buildNode :: Structural -> Maybe (Action [Double]))
