{-# LANGUAGE ExistentialQuantification    #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
module Types where

import           Data.Yaml
import           Shake.BuildNode

type CaseId = String

type BTHash = String
type UKFHash = String
type TQHash = String

data T1wType
  = T1wGiven String
  | T1wXc String
  deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read,FromJSON,ToJSON)

data T2wType
  = T2wGiven String
  | T2wXc String
  deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read,FromJSON,ToJSON)

data MaskType movingtype
  = RigidMask BTHash StructuralMaskMethod movingtype
  | NormalMask StructuralMaskMethod
  deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read,FromJSON,ToJSON)

type T1wMaskType = MaskType T2wType
type T2wMaskType = MaskType T1wType

data DwiType = DwiGiven String
             | DwiHcp [Int]
             | DwiXC DwiType
             | DwiEpi DwiType DwiMaskMethod T2wType T2wMaskType BTHash
  deriving (Show, Generic, Typeable, Eq, Hashable, Binary, NFData, Read,FromJSON,ToJSON)

data DwiMaskMethod
  = DwiMaskGiven String
  | DwiMaskHcp
  | DwiMaskEpi
  deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read,FromJSON,ToJSON)


data FreeSurferType
  = FreeSurferGiven String
  | FreeSurferUsingMask T1wType T1wMaskType
  deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read,FromJSON,ToJSON)

data StructuralMaskMethod =
    StructuralMaskMabs BTHash
  | StructuralMaskGiven String
  deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read,FromJSON,ToJSON)

data FsToDwiMethod
  = FsBrain_T1_T2_B0 T1wType T2wType T1wMaskType
  | FsBrain_B0
  deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read,FromJSON,ToJSON)

type Params = [(String,String)]
data UKFTractographyType
  = UKFTractographyDefault
  | UKFTractographyCustom Params
  | UKFTractographyGiven
  deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read,FromJSON,ToJSON)

data Inputs = Inputs {
  fstypes :: [FreeSurferType]
  ,dwimaskpairs :: [(DwiType, DwiMaskMethod)]
  ,ukftypes :: [UKFTractographyType]
  ,fs2dwimethods :: [FsToDwiMethod]
  } deriving (Show, Generic)

instance ToJSON Inputs
instance FromJSON Inputs
