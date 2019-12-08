module Day08 where

import Data.Ord

import Control.Lens as L hiding (parts)
import Data.Vector (Vector)
import qualified Data.Vector as Vector

import AOC.Solution
import ParsingPrelude

solution :: Solution [Vector (Vector Pixel)] String
solution = Solution
  { decodeInput = many layer
  , parts = "ab"
  , solvePart = \case
    'a' -> Just . show . part1
    'b' -> Just . unlines . ("":) . part2
    _ -> const Nothing
  , showResult = const id
  , tests = []
  }
  where
    row = Vector.replicateM 25 (pixelFromDigit <$> digitChar)
    layer = Vector.replicateM 6 row

rows, columns :: Int
rows = 6
columns = 25

data Pixel = Black | White | Transparent
  deriving (Eq, Ord, Show)

instance Semigroup Pixel where
  Transparent <> y = y
  x <> _ = x
instance Monoid Pixel where
  mempty = Transparent

blitPixel :: Pixel -> Char
blitPixel Black = ' '
blitPixel White = '#'
blitPixel Transparent = ' '

pixelFromDigit :: Char -> Pixel
pixelFromDigit '0' = Black
pixelFromDigit '1' = White
pixelFromDigit _ = Transparent

part1 :: [Vector (Vector Pixel)] -> Int
part1 layers = blacks * transparents
  where
    leastWhites = minimumByOf folded (comparing $ lengthOf (folded.folded.only White)) layers
    blacks = lengthOf (_Just.folded.folded.only Black) leastWhites
    transparents = lengthOf (_Just.folded.folded.only Transparent) leastWhites

part2 :: [Vector (Vector Pixel)] -> [String]
part2 =
  toLines
  . (fmap.fmap) blitPixel
  . foldr
    (Vector.zipWith . Vector.zipWith $ (<>))
    (Vector.replicate rows . Vector.replicate columns $ mempty)
  where
    toLines :: Vector (Vector Char) -> [String]
    toLines = Vector.toList . fmap (Vector.toList)