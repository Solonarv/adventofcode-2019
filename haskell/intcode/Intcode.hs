module Intcode where

import Control.Monad.ST
import Data.Functor
import Data.STRef

import Control.Monad.Except
import Control.Monad.Loops (untilJust)
import Control.Monad.Primitive
import Control.Monad.Reader
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Vector.Unboxed (Vector, MVector)
import qualified Data.Vector.Unboxed as Vector
import qualified Data.Vector.Unboxed.Mutable as MVector
import Data.Conduit (ConduitT, (.|))
import qualified Data.Conduit as C
import qualified Data.Conduit.Combinators as C
import qualified Data.Conduit.Lift as C

import ParsingPrelude
import Util

data VM s = VM
  { vmIP :: STRef s Int
  , vmExited :: STRef s (Maybe String)
  , vmMem :: MVector s Int
  }

type InterpreterM m = ConduitT Int Int (ReaderT (VM (PrimState m)) m)

type MonadVM m = (MonadReader (VM (PrimState m)) m, PrimMonad m)

runVM :: PrimMonad m => Map Int SomeOp -> Vector Int -> ConduitT Int Int m String
runVM opcodeMap code = do
  vm <- newVM code
  C.runReaderC vm $ execVM opcodeMap

runVMPure :: [Int] -> Map Int SomeOp -> Vector Int -> [Int]
runVMPure inp opcodeMap code = runST do
  C.runConduit do
    C.yieldMany inp .| void (runVM opcodeMap code) .| C.sinkList

runVMio :: Map Int SomeOp -> Vector Int -> IO ()
runVMio opcodeMap code = do
  msg <- C.runConduit do
    C.repeatM (putStr "> " >> readLn)
      .| conduitConsumeAll (conduitMapMOutput print (runVM opcodeMap code))
  putStrLn ("exited with: " <> msg)

newVM :: PrimMonad m => Vector Int -> m (VM (PrimState m))
newVM code = liftST (VM <$> newSTRef 0 <*> newSTRef Nothing <*> Vector.thaw code)

setIP :: MonadVM m => Int -> m ()
setIP i = do ipr <- asks vmIP; liftST (writeSTRef ipr i)

getIP :: MonadVM m => m Int
getIP = liftST . readSTRef =<< asks vmIP

getIncrIP :: MonadVM m => m Int
getIncrIP = do i <- getIP; i <$ setIP (i+1)

data Mode = Immediate | Position

load :: MonadVM m => Mode -> m Int
load Immediate = do
  mem <- asks vmMem
  i <- getIncrIP
  MVector.read mem i
load Position = do
  mem <- asks vmMem
  i <- getIncrIP
  i' <- MVector.read mem i
  MVector.read mem i'

store :: MonadVM m => Int -> m ()
store x = do
  addr <- load Immediate
  setMem addr x

setMem :: MonadVM m => Int -> Int -> m ()
setMem i x = do
  mem <- asks vmMem
  MVector.write mem i x

exitMsg :: MonadVM m => m (Maybe String)
exitMsg = liftST . readSTRef =<< asks vmExited

exitWith :: MonadVM m => String -> m ()
exitWith msg = do
  exitRef <- asks vmExited
  liftST (writeSTRef exitRef (Just msg))

nextOp :: MonadVM m => Map Int SomeOp -> m (Either String (Stream Mode, SomeOp))
nextOp opcodeMap = do
  opcode <- load Immediate
  let (modes, code) = readOpcode opcode
      mbOp = Map.lookup code opcodeMap
  pure case mbOp of
    Nothing -> Left ("invalid opcode: " <> show code)
    Just op -> Right (modes, op)

readOpcode :: Int -> (Stream Mode, Int)
readOpcode opcode = let
  (modeKey, code) = opcode `divMod` 100
  modes = fmap (\case 0 -> Position; _ -> Immediate) $ unfoldStream (swap . (`divMod` 10)) modeKey
  in (modes, code)

execVM :: PrimMonad m => Map Int SomeOp -> InterpreterM m String
execVM opcodeMap = untilJust do
  nextOp opcodeMap >>= \case
    Left err -> exitWith err  
    Right (modes, op) -> runSomeOp op modes
  exitMsg

type Op = forall m. PrimMonad m => Stream Mode -> InterpreterM m ()

newtype SomeOp = SomeOp { runSomeOp :: Op }

class NaryOp t where
  naryOp :: t -> Op

instance NaryOp Int where
  naryOp x _ = store x

instance (arg ~ Int, NaryOp r) => NaryOp (arg -> r) where
  naryOp f (m:>>ms) = do
    x <- load m
    naryOp (f x) ms

defaultOpcodeMap :: Map Int SomeOp
defaultOpcodeMap = Map.fromList
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
    (.=) :: Int -> Op -> (Int, SomeOp)
    k .= f = (k, SomeOp f)

opOutput :: Op
opOutput (m:>>_) = C.yield =<< load m

opInput :: Op
opInput _ = C.await >>= \case
  Nothing -> exitWith "end of input"
  Just x -> store x

opJumpWhen :: (Int -> Bool) -> Op
opJumpWhen cond (mVal :>> mTarg :>>_) = do
  val <- load mVal
  targ <- load mTarg
  when (cond val) (setIP targ)

opExit :: Op
opExit _ = exitWith "halt"

parseIntcode :: Parser (Vector Int)
parseIntcode = Vector.fromList <$> (signed space decimal `sepBy` char ',')