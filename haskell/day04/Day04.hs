module Day04 where

import Control.Applicative
import Data.List

import AOC.Solution
import ParsingPrelude
import Util

solution :: Solution (Int, Int) Int
solution = Solution
  { decodeInput = liftA2 (,) decimal (char '-' *> decimal)
  , parts = "ab"
  , solvePart = \case
    'a' -> Just . length . possiblePasswords hasDoubleDigit
    'b' -> Just . length . possiblePasswords hasIsolatedDoubleDigit
    _ -> const Nothing
  , showResult = const show
  , tests = []
  }

possiblePasswords :: ([Int] -> Bool) -> (Int, Int) -> [Int]
possiblePasswords valid (lo, hi) =
  [ pw
  | pw <- [lo .. hi]
  , let digits = intDigits pw
  , isNondecreasing digits
  , valid digits
  ]

intDigits :: Int -> [Int]
intDigits = reverse . unfoldr \case
  0 -> Nothing
  i -> Just (swap $ i `divMod` 10)

hasDoubleDigit :: Eq a => [a] -> Bool
hasDoubleDigit = or . (zipWith (==) <*> tail)

isNondecreasing :: Ord a => [a] -> Bool
isNondecreasing = and . (zipWith (<=) <*> tail)

runs :: Eq a => [a] -> [Int]
runs = map length . group

hasIsolatedDoubleDigit :: Eq a => [a] -> Bool
hasIsolatedDoubleDigit digits = 2 `elem` runs digits
