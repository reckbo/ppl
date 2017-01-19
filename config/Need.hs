module Need where

import           Node

fsTypes      = [FreeSurferWithMask StructuralMaskMabs]
fs2dwiTypes  = [FsBrain_B0]
-- dwiTypes     = [DwiXC DwiGiven, DwiXC (DwiHcp [98,99])]
dwiTypes     = [DwiGiven, DwiHcp [98,99]]
dwimaskTypes = [DwiMaskHcp]
ukfTypes     = [UKFTractographyDefault]
