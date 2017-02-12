module HCP.Util
  ( readoutTime
  , posOrientation
  , hcppath
  ) where

import           HCP.Types
import           NodeUtil       (showKey)
import           Paths           (outdir)
import           System.FilePath ((<.>), (</>))

hcppath caseid stage n = outdir </> caseid </> "hcp" </> stage </> showKey n

posOrientation :: PhaseEncoding -> PhaseEncoding
posOrientation AP = PA
posOrientation LR = RL
posOrientation x = x

readoutTime :: PhaseLength -> EchoSpacing -> Float
readoutTime l echo = (echo * numPEsteps) / 1000
  where numPEsteps = fromIntegral $ l - 1
