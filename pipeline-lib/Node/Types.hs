{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
module Node.Types where

import           Shake.BuildNode

type CaseId = String

data StructuralType = T1w
                    | T2w
                    | StructuralXC StructuralType
                    deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

data DwiType = DwiGiven
             | DwiHcp [Int]
             | DwiXC DwiType
  deriving (Show, Generic, Typeable, Eq, Hashable, Binary, NFData, Read)

data DwiMaskType = DwiMaskGiven
                 | DwiMaskHcp
        deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)


data FreeSurferType = FreeSurferGiven
                    | FreeSurferFromT1Given StructuralMaskType
                    | FreeSurferFromT1XC StructuralMaskType
                    deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

-- data FreeSurferType
--   = FreeSurferGiven
--   | FreeSurferFromT1 StrctMaskAlg
  -- deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)
-- data FreeSurferType
--   = FreeSurferGiven
--   | FreeSurferWithMask StrctMaskAlg
--   deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

data StructuralMaskType = StructuralMaskMabs
                        | StructuralMaskSource
                        | StructuralMaskRigid StructuralMaskType
                        deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

data FsToDwiType = FsBrain_T1_T2_B0 StructuralType StructuralMaskType
                 | FsBrain_B0
                 deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

type Params = [(String,String)]
data UKFTractographyType
  = UKFTractographyDefault
  | UKFTractographyCustom Params
  | UKFTractographyGiven
  deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)
