module Day02 where

import Control.Monad

import Data.Vector (Vector, (//))
import qualified Data.Vector as Vector
import qualified Data.Vector.Mutable as MVector

import AOC.Solution
import ParsingPrelude
import Util

solution :: Solution (Vector Int) Int
solution = Solution
  { decodeInput = Vector.fromList <$> (decimal `sepBy` char ',')
  , parts = "ab"
  , solvePart = \case
    'a' -> Just . Vector.head . runTillHalt . replaceInitialValues 12 2
    'b' -> fmap combineNounVerb . searchForOutput 19690720 100
    _ -> const Nothing
  , showResult = const show
  , tests = []
  }


replaceInitialValues :: Int -> Int -> Vector Int -> Vector Int
replaceInitialValues noun verb vec = vec // [(1, noun), (2, verb)]

runTillHalt :: Vector Int -> Vector Int
runTillHalt = Vector.modify (go 0)
  where
    go !i vec = do
      opcode <- MVector.read vec i
      case opcode of
        1 -> op (+) i vec >> go (i+4) vec
        2 -> op (*) i vec >> go (i+4) vec
        99 -> pure () -- we're done, halt
        _other -> error "shouldn't happen"
    op f i vec = do
      x <- MVector.read vec =<< MVector.read vec (i+1)
      y <- MVector.read vec =<< MVector.read vec (i+2)
      target <- MVector.read vec (i+3)
      MVector.write vec target (f x y)

searchForOutput :: Int -> Int -> Vector Int -> Maybe (Int, Int)
searchForOutput needle searchRange program = safeHead do
  noun <- [1 .. searchRange]
  verb <- [1 .. searchRange]
  let modifiedProgram = replaceInitialValues noun verb program
      finalState = runTillHalt modifiedProgram
  (noun, verb) <$ guard (Vector.head finalState == needle)

combineNounVerb :: (Int, Int) -> Int
combineNounVerb (noun, verb) = noun * 100 + verb
