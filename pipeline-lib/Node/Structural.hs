{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
module Node.Structural
  ( StructuralType (..)
  , Structural (..)
  , rules
  ) where

import Util (convertImage)
import           Data.Maybe      (fromMaybe)
import           Node.Util
import           Shake.BuildNode

type CaseId = String

data StructuralType = T1w
                    | T2w
                    | StructuralXC StructuralType
                    deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

newtype Structural = Structural (StructuralType, CaseId)
                    deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

instance BuildNode Structural where
  path (Structural (T1w, caseid)) =  getPath "t1" caseid
  path (Structural (T2w, caseid)) =  getPath "t2" caseid
  path n@(Structural (StructuralXC _, caseid)) = outdir </> caseid </> showKey n <.> "nrrd"

  build (Structural (T1w, caseid)) = Nothing
  build (Structural (T2w, caseid)) = Nothing

  build n@(Structural (StructuralXC strcttype, caseid)) = Just $ withTempDir $ \tmpdir -> do
    let strctNrrd = tmpdir </> "strctXC.nrrd"
        strctNode = Structural (strcttype, caseid)
    need strctNode
    liftIO $ Util.convertImage (path strctNode) strctNrrd
    command_ [] "config/axis_align_nrrd.py" ["-i", path strctNode
                                            ,"-o", path n]
    command_ [] "config/center.py" ["-i", path n
                                   ,"-o", path n]


rules = rule (buildNode :: Structural -> Maybe (Action [Double]))
