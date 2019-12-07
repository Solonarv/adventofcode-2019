module Day07 where

import Control.Monad
import Data.Functor (void)
import Data.Functor.Identity
import Data.List (permutations)
import Data.Traversable

import Control.Monad.State.Strict
import Data.Conduit (ConduitT, (.|))
import qualified Data.Conduit as C
import qualified Data.Conduit.Combinators as C
import qualified Data.Conduit.Lift as C
import Data.Vector.Unboxed (Vector)

import AOC.Solution
import Intcode
import Util

solution :: Solution (Vector Int) Int
solution = Solution
  { decodeInput = parseIntcode
  , parts = "ab"
  , solvePart = \case
    'a' -> join . safeMaximum . tryAllPhaseSettings 5
    'b' -> join . safeMaximum . tryAllPhaseSettingsLoopy [5..9]
    _ -> const Nothing
  , showResult = const show
  , tests =
    [ "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0" :=>
      [ ('a', "43210")
      ]
    , "3,23,3,24,1002,24,10,24,1002,23,-1,23,\
      \101,5,23,23,1,24,23,23,4,23,99,0,0" :=>
      [ ('a', "54321")
      ]
    , "3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,\
      \1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0" :=>
      [ ('a', "65210")
      ]
    , "3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,\
      \27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5" :=>
      [ ('b', "139629729")
      ]
    , "3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,\
      \-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,\
      \53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10" :=>
      [ ('b', "18216")
      ]
    ]
  }

tryAllPhaseSettings :: Int -> Vector Int -> [Maybe Int]
tryAllPhaseSettings n code = [finalOutput code phases | phases <- permutations [0 .. n-1]]

finalOutput :: Vector Int -> [Int] -> Maybe Int
finalOutput code phases = let

  program :: ConduitT Int Int Identity ()
  program = void (runVM defaultOpcodeMap code)

  amplifiers :: [ConduitT Int Int Identity ()]
  amplifiers = C.yield 0 : repeat program

  stages :: [ConduitT Int Int Identity ()]
  stages = zipWith (\ph amp -> C.yield ph >> amp) phases amplifiers

  pipeline :: ConduitT Int Int Identity ()
  pipeline = foldr (.|) program stages
  in C.runConduitPure (pure () .| pipeline .| C.await)

tryAllPhaseSettingsLoopy :: [Int] -> Vector Int -> [Maybe Int]
tryAllPhaseSettingsLoopy phs code = finalPowerLoopy code <$> permutations phs

finalPowerLoopy :: Vector Int -> [Int] -> Maybe Int
finalPowerLoopy code phases = let
  program :: InterpreterM ()
  program = execVM defaultOpcodeMap

  initialVMs :: [VMState]
  initialVMs = [C.runConduitPure $ C.execStateC (newVM code) $ C.yield ph .| program .| C.sinkNull | ph <- phases]

  go :: [Int] -> State [VMState] [Int]
  go inputs = do
    vms <- get
    if all (not . vmExited) vms
      then let
        (outs, vms') = mapAccumL
          do \vs vm ->
              let
              pipe = C.yieldMany vs .| program .| C.sinkList
              in C.runConduitPure (C.runStateC (resumeVM vm) pipe)
          (inputs)
          vms
        in put vms' >> go outs
      else pure inputs
  in safeHead $ flip evalState initialVMs $ go [0]
