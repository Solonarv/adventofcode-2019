module Day01 where

import AOC.Solution
import ParsingPrelude
import Util

solution :: Solution [Int] Int
solution = Solution
  { decodeInput = decimal `sepBy` space1
  , parts = "ab"
  , solvePart = \case
    'a' -> Just . totalFuelRequirements
    'b' -> Just . tyrannicFuelRequirements
    _ -> const Nothing
  , showResult = const show
  , tests =
    [ "12" :=>
      [ ('a', "2")
      , ('b', "2")
      ]
    , "14" :=> 
      [ ('a', "2")
      , ('b', "2")
      ]
    , "1969" :=>
      [ ('a', "654")
      , ('b', "966")
      ]
    , "100756" :=>
      [ ('a', "33583")
      , ('b', "50346")
      ]
    ]
  }

fuelForMass :: Int -> Int
fuelForMass = subtract 2 . (`div` 3)

totalFuelRequirements :: [Int] -> Int
totalFuelRequirements = sum' . map fuelForMass

tyrannicFuelRequirements :: [Int] -> Int
tyrannicFuelRequirements = sum' . map tyrannicFuelEquation

tyrannicFuelEquation :: Int -> Int
tyrannicFuelEquation = sum' . takeWhile (> 0) . drop 1 . iterate fuelForMass