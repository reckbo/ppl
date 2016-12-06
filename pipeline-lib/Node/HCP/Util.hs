module Node.HCP.Util
  ( readoutTime
  , posOrientation
  , hcpdir
  ) where

import           Node.HCP.Types
import           Paths           (outdir)
import           System.FilePath ((<.>), (</>))

hcpdir caseid stage = outdir </> caseid </> "hcp" </> stage

posOrientation :: PhaseEncoding -> PhaseEncoding
posOrientation AP = PA
posOrientation LR = RL
posOrientation x = x

readoutTime :: PhaseLength -> EchoSpacing -> Float
readoutTime l echo = (echo * numPEsteps) / 1000
  where numPEsteps = fromIntegral $ l - 1
