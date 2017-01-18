module Need where

import           Node

fsTypes      = [FreeSurferFromT1XC StructuralMaskMabs]
fs2dwiTypes  = [FsBrain_B0]
dwiTypes     = [DwiXC DwiGiven, DwiXC (DwiHcp [98,99])]
dwimaskTypes = [DwiMaskHcp]
ukfTypes     = [UKFTractographyDefault]
