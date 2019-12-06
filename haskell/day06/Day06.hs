module Day06 where

import Control.Monad
import Data.Maybe

import Data.Map (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Map.Lazy as LazyMap

import AOC.Solution
import ParsingPrelude
import Util

solution :: Solution (Map String (Int, Map String Int)) Int
solution = Solution
  { decodeInput = orbitAncestors . Map.fromList <$> ((flip (,) <$> word <* char ')' <*> word) `sepBy` space)
  , parts = "ab"
  , solvePart = \case
    'a' -> Just . totalOrbits
    'b' -> shortestTransfer "YOU" "SAN"
    _ -> const Nothing
  , showResult = const show
  , tests =
    [ unlines ["COM)B","B)C","C)D","D)E","E)F","B)G","G)H","D)I","E)J","J)K","K)L"] :=>
      [ ('a', "42")
      ]
    , unlines ["COM)B","B)C","C)D","D)E","E)F","B)G","G)H","D)I","E)J","J)K","K)L","K)YOU","I)SAN"] :=>
      [ ('b', "4")
      ]
    ]
  }
  where
    word = some alphaNumChar

totalOrbits :: Map String (Int, Map String Int) -> Int
totalOrbits = sum' . fmap fst

orbitAncestors :: Map String String -> Map String (Int, Map String Int)
orbitAncestors orbitMap = ancestors
  where
    ancestors = flip LazyMap.map
      orbitMap
      \parent ->
        let
          ~(depth, distances) = fromMaybe (0, Map.empty) (Map.lookup parent ancestors)
        in
          ( depth + 1
          , Map.insert parent 1 $ Map.map (+1) distances
          )

shortestTransfer :: String -> String -> Map String (Int, Map String Int) -> Maybe Int
shortestTransfer src tgt orbits = do
  srcAncestors <- snd <$> Map.lookup src orbits
  tgtAncestors <- snd <$> Map.lookup tgt orbits
  let commonAncestors = Map.intersectionWith (+) srcAncestors tgtAncestors
  guard . not . null $ commonAncestors
  pure (minimum commonAncestors - 2)
