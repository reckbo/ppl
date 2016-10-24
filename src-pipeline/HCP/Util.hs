module HCP.Util
  ( posPhase
  , negPhase
  , readoutTime
  ) where

import HCP.Types

negPhase :: PhaseEncoding -> PhaseEncoding
negPhase PA = AP
negPhase AP = AP
negPhase RL = LR
negPhase LR = LR

posPhase :: PhaseEncoding -> PhaseEncoding
posPhase PA = PA
posPhase AP = PA
posPhase RL = RL
posPhase LR = RL

readoutTime :: PhaseLength -> EchoSpacing -> Float
readoutTime l echo = (echo * numPEsteps) / 1000
  where numPEsteps = fromIntegral $ l - 1
