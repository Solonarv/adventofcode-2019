module Day06 where

import Data.Maybe

import Data.Map (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Map.Lazy as LazyMap

import AOC.Solution
import ParsingPrelude
import Util

solution :: Solution (Map String String) Int
solution = Solution
  { decodeInput = Map.fromList <$> ((flip (,) <$> word <*> word) `sepBy` space)
  , parts = "a"
  , solvePart = \case
    'a' -> Just . totalOrbits
    _ -> const Nothing
  , showResult = const show
  , tests =
    [ unlines ["COM)B","B)C","C)D","D)E","E)F","B)G","G)H","D)I","E)J","J)K","K)L"] :=>
      [ ('a', "42")
      ]
    ]
  }
  where
    word = many alphaNumChar

totalOrbits :: Map String String -> Int
totalOrbits orbitMap = sum parentCounts
  where
    parentCounts = flip LazyMap.mapWithKey
      orbitMap
      \_child parent ->
        1 + fromMaybe 0 (LazyMap.lookup parent parentCounts)
