{-# LANGUAGE ExistentialQuantification    #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
module Node.Types where

import           Shake.BuildNode

type CaseId = String

-- data StructuralType = T1w
--                     | T2w
--                     | StructuralXC StructuralType
                    -- deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

type BTHash = String
type UKFHash = String
type TQHash = String

data T1wType
  = T1wGiven
  | T1wXc
  deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

data T2wType
  = T2wGiven
  | T2wXc
  deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

data MaskType movingtype
  = RigidMask BTHash StructuralMaskMethod movingtype
  | NormalMask StructuralMaskMethod
  deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

type T1wMaskType = MaskType T2wType
type T2wMaskType = MaskType T1wType

-- data StructuralType
--   = T1wStructural T1wType
--   | T2wStructural T2wType
--   deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

data DwiType = DwiGiven
             | DwiHcp [Int]
             | DwiXC DwiType
             | DwiEpi DwiType DwiMaskMethod T2wType StructuralMaskMethod
  deriving (Show, Generic, Typeable, Eq, Hashable, Binary, NFData, Read)

data DwiMaskMethod
  = DwiMaskGiven
  | DwiMaskHcp
  deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)


data FreeSurferType
  = FreeSurferGiven
  | FreeSurferUsingMask T1wType T1wMaskType
  deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

data StructuralMaskMethod =
    StructuralMaskMabs BTHash
  | StructuralMaskGiven
  -- | StructuralMaskRigid BTHash StructuralMaskMethod StructuralType
  deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

data FsToDwiMethod
  = FsBrain_T1_T2_B0 T1wType T2wType T1wMaskType
  | FsBrain_B0
  deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

type Params = [(String,String)]
data UKFTractographyType
  = UKFTractographyDefault
  | UKFTractographyCustom Params
  | UKFTractographyGiven
  deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)
