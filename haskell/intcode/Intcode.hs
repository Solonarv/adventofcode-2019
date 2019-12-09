module Intcode where

import Data.Functor
import Data.Functor.Identity

import Control.Monad.Except
import Control.Monad.Loops (whileM_)
import Control.Monad.State.Strict
import Control.Lens
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as Vector
import Data.Conduit (ConduitT, (.|))
import qualified Data.Conduit as C
import qualified Data.Conduit.Combinators as C
import qualified Data.Conduit.Lift as C

import PagedVector
import ParsingPrelude
import Util

data VMState = VM
  { vmIP :: Int
  , vmRelBase :: Int
  , vmStatus :: VMStatus
  , vmMem :: Paged Vector Int
  }
  deriving (Eq, Show)

_vmIP :: Lens' VMState Int
_vmIP f vm = f (vmIP vm) <&> \i -> vm{vmIP=i}

_vmRelBase :: Lens' VMState Int
_vmRelBase f vm = f (vmRelBase vm) <&> \i -> vm{vmRelBase = i}

_vmStatus :: Lens' VMState VMStatus
_vmStatus f vm = f (vmStatus vm) <&> \s -> vm{vmStatus = s}

_vmMem :: Lens' VMState (Paged Vector Int)
_vmMem f vm = f (vmMem vm) <&> \m -> vm{vmMem = m}

data VMStatus = Finished String | Running | AwaitingInput Int
  deriving (Eq, Ord, Show)

type InterpreterT m = ConduitT Int Int (StateT VMState m)

type InterpreterM = InterpreterT Identity

type MonadVM m = MonadState VMState m

type Code = Vector Int

runVM :: Monad m => Map Int SomeOp -> Code -> ConduitT Int Int m VMState
runVM opcodeMap code = 
  C.execStateC (newVM code) $ execVM opcodeMap

runVMPure :: [Int] -> Map Int SomeOp -> Code -> [Int]
runVMPure inp opcodeMap code =
  C.runConduitPure do
    C.yieldMany inp .| void (runVM opcodeMap code) .| C.sinkList

runVMio :: Map Int SomeOp -> Code -> IO ()
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

newVM :: Code -> VMState
newVM code = VM 0 0 Running (mkPaged 512 0 code)

setIP :: MonadVM m => Int -> m ()
setIP = assign _vmIP

getIP :: MonadVM m => m Int
getIP = use _vmIP

getIncrIP :: MonadVM m => m Int
getIncrIP = _vmIP <<+= 1

data Mode = Immediate | Position | Relative

load :: MonadVM m => Mode -> m Int
load Immediate = do
  i <- getIncrIP
  use (_vmMem . atAddr i)
load Position = do
  i' <- load Immediate
  use (_vmMem . atAddr i')
load Relative = do
  offset <- load Immediate
  base <- use _vmRelBase
  use (_vmMem . atAddr (offset + base))

store :: MonadVM m => Mode -> Int -> m ()
store Immediate _ = exitWith "write parameters cannot be in immediate mode!"
store Position x = do
  addr <- load Immediate
  setMem addr x
store Relative x = do
  offset <- load Immediate
  base <- use _vmRelBase
  setMem (offset + base) x

setMem :: MonadVM m => Int -> Int -> m ()
setMem i x = _vmMem . atAddr i .= x

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
    Just oper -> Right (modes, oper)

readOpcode :: Int -> (Stream Mode, Int)
readOpcode opcode = let
  (modeKey, code) = opcode `divMod` 100
  readMode = \case
    0 -> Position
    1 -> Immediate
    2 -> Relative
    e -> error $ "invalid parameter mode: " <> show e
  modes = readMode <$> unfoldStream (swap . (`divMod` 10)) modeKey
  in (modes, code)

execVM :: Monad m => Map Int SomeOp -> InterpreterT m ()
execVM opcodeMap = whileM_
  do
    status <- gets vmStatus
    pure (status == Running)
  do
    nextOp opcodeMap >>= \case
      Left err -> exitWith err  
      Right (modes, oper) -> runSomeOp oper modes

type Op = forall m. Monad m => Stream Mode -> InterpreterT m ()

newtype SomeOp = SomeOp { runSomeOp :: Op }

class NaryOp t where
  naryOp :: t -> Op

instance NaryOp Int where
  naryOp x (m:>>_) = store m x

instance (arg ~ Int, NaryOp r) => NaryOp (arg -> r) where
  naryOp f (m:>>ms) = do
    x <- load m
    naryOp (f x) ms

defaultOpcodeMap :: Map Int SomeOp
defaultOpcodeMap = Map.fromList
  [ 1  ==> naryOp (+)
  , 2  ==> naryOp (*)
  , 3  ==> opInput
  , 4  ==> opOutput
  , 5  ==> opJumpWhen (/= 0)
  , 6  ==> opJumpWhen (== 0)
  , 7  ==> naryOp (\x y -> fromEnum (x < y))
  , 8  ==> naryOp (\x y -> fromEnum (x == y))
  , 9  ==> opAdjustRelBase
  , 99 ==> opExit
  ] where
    (==>) :: Int -> Op -> (Int, SomeOp)
    k ==> f = (k, SomeOp f)

opOutput :: Op
opOutput (m:>>_) = C.yield =<< load m

opInput :: Op
opInput (m:>>_) = C.await >>= \case
  Nothing -> modify \vm -> vm{ vmStatus = AwaitingInput (vmIP vm - 1) }
  Just x -> store m x

opJumpWhen :: (Int -> Bool) -> Op
opJumpWhen cond (mVal :>> mTarg :>>_) = do
  val <- load mVal
  targ <- load mTarg
  when (cond val) (setIP targ)

opAdjustRelBase :: Op
opAdjustRelBase (m:>>_) = do
  adj <- load m
  _vmRelBase += adj

opExit :: Op
opExit _ = exitWith "halt"

parseIntcode :: Parser (Code)
parseIntcode = Vector.fromList <$> (signed space decimal `sepBy` char ',')