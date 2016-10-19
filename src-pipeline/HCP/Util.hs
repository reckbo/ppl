{-# LANGUAGE FlexibleInstances #-}
module HCP.Util
  ( mkDWIPair
   , readoutTime
   , writeB0s
   , mkIndexList
   , readDWIPair
  ) where

import           Data.Function
import           Data.List
import           Development.Shake
import           FSL
import           HCP.Config
import           HCP.Types         (DWIInfo (..), DWIPair (..), DirType (..))

type EchoSpacing = Float
type PhaseLength = Int
type DWI = FilePath

readDWIPair :: (Int, DWI, DWI) -> Action DWIPair
readDWIPair (pid, dwi, dwi') = mkDWIPair <$>
                                pure pid <*>
                                pure dwi <*>
                                pure dwi' <*>
                                readbval (tobval dwi) <*>
                                readbval (tobval dwi')

readoutTime :: PhaseLength -> EchoSpacing -> Float
readoutTime l echo = (echo * numPEsteps) / 1000
  where numPEsteps = fromIntegral $ l - 1

getValidB0Indices :: [BValue] -> [Int]
getValidB0Indices bs = reverse $ foldl' f [i0] indices
  where
    f (i:is) i' = if (i' - i) >= b0dist
                     then i':i:is
                     else i:is
    f _ _ = error "getValidB0Indices: DWI must have at least two b-values."
    (i0:indices) = findIndices (< b0maxbval) bs

mkDWIInfo :: Int -> DirType -> DWI -> [BValue] -> Int -> DWIInfo
mkDWIInfo pid dirtype dwi bs matchingLength
  = DWIInfo
  pid
  dirtype
  dwi
  (length bs)
  (findIndices (< b0maxbval) bs)
  (getValidB0Indices bs)
  (filter (< matchingLength) $ getValidB0Indices bs)

mkDWIPair :: Int -> DWI -> DWI -> [BValue] -> [BValue] -> DWIPair
mkDWIPair pid dwi dwi' bs bs'
  = DWIPair
  (mkDWIInfo pid Pos dwi bs matchingLength)
  (mkDWIInfo pid Neg dwi' bs' matchingLength)
  where matchingLength = (min`on`length) bs bs'

writeB0s :: FilePath -> [DWIInfo] -> Action ()
writeB0s out dwiinfos =
  do fs <- traverse writeB0 dwiinfos
     mergeVols out fs
     trimVol out
  where
    writeB0 dwiinfo = extractVols (_dwi dwiinfo) (_b0indicesToUse dwiinfo)

mkIndexList :: [DWIPair] -> [Int]
mkIndexList dwipairs = mkIndex' $ addLast b0indices size
  where
    posSizes = map (_size . _pos) dwipairs
    negSizes = map (_size . _neg) dwipairs
    sizes = scanl (+) 0 $ posSizes ++ negSizes
    size = head . reverse $ sizes
    posb0indices = map (_b0indicesToUse . _pos) dwipairs
    negb0indices = map (_b0indicesToUse . _neg) dwipairs
    b0indices = concat $ zipWith (\is sz -> map (+sz) is) (posb0indices++negb0indices) sizes
    mkIndex' is = reverse $ foldl g [] is
      where g res i =
              let dx = i - length res
                  val = case res of
                    [] -> 1
                    _ -> 1 + head res
              in (replicate dx val) ++ res

addLast :: [a] -> a -> [a]
addLast xs y = reverse . (y:) . reverse $ xs
