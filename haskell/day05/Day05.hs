module Day05 where

import Data.Vector.Unboxed (Vector)

import AOC.Solution
import Intcode

solution :: Solution (Vector Int) [Int]
solution = Solution
  { decodeInput = parseIntcode
  , parts = "ab"
  , solvePart = \case
    'a' -> Just . runVMPure [1] defaultOpcodeMap
    'b' -> Just . runVMPure [5] defaultOpcodeMap
    _ -> const Nothing
  , showResult = const show
  , tests = [] -- :(
  }