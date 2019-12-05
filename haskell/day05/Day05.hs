module Day05 where

import Data.STRef

import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Vector.Unboxed (Vector, MVector)
import qualified Data.Vector.Unboxed as Vector
import qualified Data.Vector.Unboxed.Mutable as MVector

import AOC.Solution
import ParsingPrelude

solution :: Solution (Vector Int) Int
solution = Solution
  { decodeInput = Vector.fromList <$> (signed decimal `sepBy` char ',')
  , parts = "ab"
  , solvePart = \case
    'a' -> diagnosticCode . runWithInput [1]
    _ -> const Nothing
  , showResult = const show
  }

diagnosticCode :: [Int] -> Maybe Int
diagnosticCode (x:xs) = x <$ guard (all (==0) xs)
diagnosticCode [] = Nothing

runWithInput :: [Int] -> Vector Int -> [Int]
runWithInput inp prog = runST do
  vm <- newIntcodeVM inp prog
  runIntcodeVM vm
  readSTRef (outBuf vm)

data IntcodeVM s = IntcodeVM
  { inBuf :: STRef s [Int]
  , outBuf :: STRef s [Int]
  , mem :: MVector s Int
  }

newIntcodeVM :: [Int] -> Vector Int -> ST s (IntcodeVM s)
newIntcodeVM inp prog = do
  mem <- Vector.thaw prog
  inBuf <- newSTRef inp
  outBuf <- newSTRef []
  pure IntcodeVM{mem, inpBuffer, outBuffer}

runIntcodeVM :: IntcodeVM s -> ST s ()
runIntcodeVM vm = loop 0
  where
    loop i = do
      opcode <- MVector.read (mem vm) i
      case readOp opcode of
        Nothing -> error $ "invalid opcode " <> show opcode <>" at " <> i
        Just (OpExit, _) -> pure ()
        Just (Op arity runOp, modes) -> do
          runOp modes vm (i+1)
          loop (i + 1 + arity)

data Op = OpExit | Op !Int (forall s. [Bool] -> Int -> IntcodeVM s -> ST s ())

readOp :: Int -> Maybe Op
readOp opcode = let
  (modeKey, code) = opcode `divMod` 100
  modes = unfoldr (\n -> Just . swap . (`divMod` 100)) modeKey
  in (,) <$> Map.lookup opcode opcodeMap <*> modes

opcodeMap :: Map Int Op
opcodeMap = Map.fromList
  [ 1 .= binaryOp (+)
  , 2 .= binaryOp (*)
  , 3 .= opInput
  , 4 .= opOutput
  , 99 .= OpExit
  ] where (.=) = (,)
