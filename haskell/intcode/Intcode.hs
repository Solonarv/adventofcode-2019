module Intcode where

import Data.Functor
import Data.Functor.Identity

import Control.Monad.Except
import Control.Monad.Loops (whileM_)
import Control.Monad.State.Strict
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Vector.Unboxed (Vector, (!), (//))
import qualified Data.Vector.Unboxed as Vector
import Data.Conduit (ConduitT, (.|))
import qualified Data.Conduit as C
import qualified Data.Conduit.Combinators as C
import qualified Data.Conduit.Lift as C

import ParsingPrelude
import Util

data VMState = VM
  { vmIP :: Int
  , vmStatus :: VMStatus
  , vmMem :: Vector Int
  }
  deriving (Eq, Ord, Show)

data VMStatus = Finished String | Running | AwaitingInput Int
  deriving (Eq, Ord, Show)

type InterpreterT m = ConduitT Int Int (StateT VMState m)

type InterpreterM = InterpreterT Identity

type MonadVM m = MonadState VMState m

runVM :: Monad m => Map Int SomeOp -> Vector Int -> ConduitT Int Int m VMState
runVM opcodeMap code = 
  C.execStateC (newVM code) $ execVM opcodeMap

runVMPure :: [Int] -> Map Int SomeOp -> Vector Int -> [Int]
runVMPure inp opcodeMap code =
  C.runConduitPure do
    C.yieldMany inp .| void (runVM opcodeMap code) .| C.sinkList

runVMio :: Map Int SomeOp -> Vector Int -> IO ()
runVMio opcodeMap code = go (newVM code) []
  where
    go vm ins = do
      let (outs, vm') = C.runConduitPure $ C.runStateC vm (C.yieldMany ins .| execVM opcodeMap .| C.sinkList)
      forM_ outs \x -> putStrLn $ " = " <> show x
      case vmStatus vm' of
        Finished msg -> putStrLn $ "exited: " <> msg
        Running -> putStrLn "interpreter exited but is still running?? strange!"
        AwaitingInput _ -> do
          putStr " > "
          i <- readLn
          go (resumeVM vm') [i]

resumeVM :: VMState -> VMState
resumeVM vm = case vmStatus vm of
  AwaitingInput ip -> vm { vmIP = ip, vmStatus = Running }
  _ -> vm
  
vmExited :: VMState -> Bool
vmExited vm = case vmStatus vm of
  Finished _ -> True
  _ -> False

newVM :: Vector Int -> VMState
newVM code = VM 0 Running code

setIP :: MonadVM m => Int -> m ()
setIP i = modify \vm -> vm{vmIP = i}

getIP :: MonadVM m => m Int
getIP = gets vmIP

getIncrIP :: MonadVM m => m Int
getIncrIP = do i <- getIP; i <$ setIP (i+1)

data Mode = Immediate | Position

load :: MonadVM m => Mode -> m Int
load Immediate = do
  mem <- gets vmMem
  i <- getIncrIP
  pure (mem ! i)
load Position = do
  mem <- gets vmMem
  i <- getIncrIP
  pure (mem ! (mem ! i))

store :: MonadVM m => Int -> m ()
store x = do
  addr <- load Immediate
  setMem addr x

setMem :: MonadVM m => Int -> Int -> m ()
setMem i x = do
  mem <- gets vmMem
  modify \vm -> vm{ vmMem = mem // [(i, x)] }

exitMsg :: MonadVM m => m (Maybe String)
exitMsg = gets vmStatus <&> \case
  Finished msg -> Just msg
  _ -> Nothing

exitWith :: MonadVM m => String -> m ()
exitWith msg = modify \vm -> vm{vmStatus = Finished msg}

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

execVM :: Monad m => Map Int SomeOp -> InterpreterT m ()
execVM opcodeMap = whileM_
  do
    status <- gets vmStatus
    pure (status == Running)
  do
    nextOp opcodeMap >>= \case
      Left err -> exitWith err  
      Right (modes, op) -> runSomeOp op modes

type Op = forall m. Monad m => Stream Mode -> InterpreterT m ()

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
  Nothing -> modify \vm -> vm{ vmStatus = AwaitingInput (vmIP vm - 1) }
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