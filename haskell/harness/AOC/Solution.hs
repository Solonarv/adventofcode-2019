module AOC.Solution where

import Data.Void (Void)

import Advent (Part(..))
import Text.Megaparsec (Parsec, empty)

data Solution i a b1 b2 = Solution
  { decodeInput :: Parsec Void String a
  , preprocess  :: a -> Maybe i
  , solvePart1  :: i -> Maybe b1
  , solvePart2  :: i -> Maybe b2
  , solutionToString :: Either b1 b2 -> String
  , tests       :: [Test a i b1 b2]
  }

defSolution :: (Show b1, Show b2) => Solution a a b1 b2
defSolution = Solution
  { decodeInput = empty
  , preprocess = Just
  , solvePart1 = const Nothing
  , solvePart2 = const Nothing
  , solutionToString = either show show
  , tests = []
  }

data Test i a b1 b2 where
  ParsesAs :: Eq a => String -> a -> Test i a b1 b2
  PreprocessesAs :: Eq i => a -> i -> Test i a b1 b2
  SolvesTo1 :: (Eq b1) => i -> b1 -> Test i a b1 b2
  SolvesTo2 :: (Eq b2) => i -> b2 -> Test i a b1 b2