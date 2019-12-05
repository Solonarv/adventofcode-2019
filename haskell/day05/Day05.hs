module Day05 where

import Control.Monad
import Control.Monad.ST
import Data.Functor
import Data.STRef

import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Vector.Unboxed (Vector, MVector)
import qualified Data.Vector.Unboxed as Vector
import qualified Data.Vector.Unboxed.Mutable as MVector

import AOC.Solution
import ParsingPrelude
import Util

solution :: Solution (Vector Int) Int
solution = Solution
  { decodeInput = Vector.fromList <$> (signed space decimal `sepBy` char ',')
  , parts = "ab"
  , solvePart = \case
    'a' -> diagnosticCode . runWithInput [1]
    'b' -> safeHead . runWithInput [5]
    _ -> const Nothing
  , showResult = const show
  , tests = [] -- :(
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
  pure IntcodeVM{mem, inBuf, outBuf}

data Mode = Immediate | Position

load :: IntcodeVM s -> Mode -> Int -> ST s Int
load vm Immediate i =  MVector.read (mem vm) i
load vm Position i  = MVector.read (mem vm) =<< MVector.read (mem vm) i

store :: IntcodeVM s -> Mode -> Int -> Int -> ST s ()
store vm Immediate i x = MVector.write (mem vm) i x
store vm Position i x = do p <- MVector.read (mem vm) i; MVector.write (mem vm) p x

popInput :: IntcodeVM s -> ST s Int
popInput vm = do
  (x : xs) <- readSTRef (inBuf vm)
  x <$ writeSTRef (inBuf vm) xs

pushOutput :: IntcodeVM s -> Int -> ST s ()
pushOutput vm x = modifySTRef' (outBuf vm) (x:)

runIntcodeVM :: IntcodeVM s -> ST s ()
runIntcodeVM vm = loop 0
  where
    loop i
      | i < 0 = pure ()
      | otherwise = do
        opcode <- load vm Immediate i
        case readOp opcode of
          Nothing -> error $ "invalid opcode " <> show opcode <> " at " <> show i
          Just (Op modes runOp) -> do
            loop =<< runOp modes (i+1) vm

data Op = Op (Stream Mode) (forall s. Stream Mode -> Int -> IntcodeVM s -> ST s Int)

readOp :: Int -> Maybe Op
readOp opcode = let
  (modeKey, code) = opcode `divMod` 100
  modes = fmap (\case 0 -> Position; _ -> Immediate) $ unfoldStream (swap . (`divMod` 10)) modeKey
  in Map.lookup code opcodeMap <&> \(SomeOp f) -> Op modes f

newtype SomeOp = SomeOp { runSomeOp :: (forall s. Stream Mode -> Int -> IntcodeVM s -> ST s Int) }

opcodeMap :: Map Int SomeOp
opcodeMap = Map.fromList
  [ 1  .= naryOp (+)
  , 2  .= naryOp (*)
  , 3  .= opInput
  , 4  .= opOutput
  , 5  .= opJumpWhen (/= 0)
  , 6  .= opJumpWhen (== 0)
  , 7  .= naryOp (\x y -> fromEnum (x < y))
  , 8  .= naryOp (\x y -> fromEnum (x == y))
  , 99 .= opExit
  ] where
    (.=) :: Int -> (forall s. Stream Mode -> Int -> IntcodeVM s -> ST s Int) -> (Int, SomeOp)
    k .= f = (k, SomeOp f)

class NAryOp r where
  naryOp :: r -> Stream Mode -> Int -> IntcodeVM s -> ST s Int

instance NAryOp Int where
  naryOp x _ i vm = do
    target <- load vm Immediate i
    MVector.write (mem vm) target x
    pure (i+1)

instance (a ~ Int, NAryOp r) => NAryOp (a -> r) where
  naryOp f (mode :>> ms) i vm = do
    x <- load vm mode i
    naryOp (f x) ms (i+1) vm

opInput :: Stream Mode -> Int -> IntcodeVM s -> ST s Int
opInput _ i vm = do
  x <- popInput vm
  store vm Position i x
  pure (i+1)

opOutput :: Stream Mode -> Int -> IntcodeVM s -> ST s Int
opOutput (m:>>_) i vm = do
  x <- load vm m i
  pushOutput vm x
  pure (i+1)

opExit :: Stream Mode -> Int -> IntcodeVM s -> ST s Int
opExit _ _ _ = pure (-1)

opJumpWhen :: (Int -> Bool) -> Stream Mode -> Int -> IntcodeVM s -> ST s Int
opJumpWhen cond (mx :>> mTarg :>> _) i vm = do
  x <- load vm mx i
  if cond x
    then load vm mTarg (i+1)
    else pure (i+2)