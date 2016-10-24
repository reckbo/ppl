{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
module HCP.B0sPair
  ( mkB0sPair
   -- , writeB0s
   , mkIndexList
   -- , readDWIPair
   , B0sInfo (..)
   , B0sPair (..)
  ) where

import           Data.Function
import           Data.List
import           Data.Yaml
import           Development.Shake
import           FSL
import           GHC.Generics
import           HCP.Config
import           HCP.Types         (PhaseOrientation (..))

data B0sInfo = B0sInfo
    {_size                 :: Int
    ,_b0indices            :: [Int]
    ,_b0indicesWithMinDist :: [Int]
    ,_b0indicesToUse       :: [Int]
    }
  deriving (Show, Generic)

instance ToJSON PhaseOrientation
instance ToJSON B0sInfo
instance FromJSON B0sInfo
instance FromJSON PhaseOrientation

data B0sPair = B0sPair
  { _pos :: B0sInfo
  , _neg :: B0sInfo }
  deriving (Show, Generic)

instance ToJSON B0sPair
instance FromJSON B0sPair

-- readDWIPair :: (Int, DWI, DWI) -> ReaderT Action DWIPair
-- readDWIPair (pid, dwi, dwi') = return $
--     mkDWIPair <$>
--       pure pid <*>
--       pure dwi <*>
--       pure dwi' <*>
--       readbval (tobval dwi) <*>
--       readbval (tobval dwi')

mkB0sPair :: BValue -> Int -> [BValue] -> [BValue] -> B0sPair
mkB0sPair b0maxbval b0dist bs bs'
  = B0sPair
    (mkB0sInfo bs matchingLength)
    (mkB0sInfo bs' matchingLength)
  where
    matchingLength = (min`on`length) bs bs'
    mkB0sInfo bs matchingLength = B0sInfo
        (length bs)
        (findIndices (< b0maxbval) bs)
        (getValidB0Indices b0maxbval b0dist bs)
        (filter (< matchingLength) $ getValidB0Indices b0maxbval b0dist bs)

getValidB0Indices :: BValue -> Int -> [BValue] -> [Int]
getValidB0Indices b0maxbval b0dist bs
  = reverse $ foldl' f [i0] indices
  where
    f (i:is) i' = if (i' - i) >= b0dist
                     then i':i:is
                     else i:is
    f _ _ = error "getValidB0Indices: DWI must have at least two b-values."
    (i0:indices) = findIndices (< b0maxbval) bs

-- writeB0s :: FilePath -> [B0sInfo] -> Action ()
-- writeB0s out dwiinfos =
--   do fs <- traverse writeB0 dwiinfos
--      mergeVols out fs
--      trimVol out
--   where
--     writeB0 dwiinfo = extractVols (_dwi dwiinfo) (_b0indicesToUse dwiinfo)

mkIndexList :: [B0sPair] -> [Int]
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
