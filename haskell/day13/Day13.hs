{-# LANGUAGE TemplateHaskell #-}
module Day13 where

import Data.Foldable
import Data.Maybe
import Data.Void
import Debug.Trace

import Control.Lens hiding (parts)
import Control.Monad.State.Strict
import Data.Conduit (ConduitT, (.|))
import qualified Data.Conduit as C
import qualified Data.Conduit.Combinators as C
import qualified Data.Conduit.Lift as C
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Linear.V2

import AOC.Solution
import Intcode

data Tile = Wall | Block | Paddle | Ball
  deriving (Eq, Ord, Show)

data GameWorld = GameWorld
  { _gwGrid :: Map (V2 Int) Tile
  , _gwPaddlePos :: V2 Int
  , _gwBallPos :: V2 Int
  , _gwScore :: Int
  }

makeLenses ''GameWorld

solution :: Solution Code Int
solution = Solution
  { decodeInput = parseIntcode
  , parts = "ab"
  , solvePart = \case
    'a' -> Just . countBlockTiles
    'b' -> Just . playTillEnd
    _ -> const Nothing
  , showResult = const show
  , tests = []
  }

tileFromInt :: Int -> Maybe Tile
tileFromInt 1 = Just Wall
tileFromInt 2 = Just Block
tileFromInt 3 = Just Paddle
tileFromInt 4 = Just Ball
tileFromInt _ = Nothing

chunker :: Monad m => ConduitT Int (V2 Int, Int) m ()
chunker = C.await >>= traverse_ \x ->
  C.await >>= traverse_ \y ->
    C.await >>= traverse_ \tileId -> do
      C.yield (V2 x y,  tileId)
      chunker

countBlockTiles :: Code -> Int
countBlockTiles code = let
  interp = execVM defaultOpcodeMap
  initState = newVM code

  screen = C.foldl
    (\scr dat -> case dat of
      (pos, tileId) -> scr & at pos .~ tileFromInt tileId
    )
    Map.empty
  result = C.runConduitPure $
    C.evalStateC initState (pure () .| interp .| chunker .| screen)
  in length [t | (_, t) <- Map.toAscList result, t == Block]

-- Naive AI player that just moves paddle towards ball at all times
naiveAI :: MonadState GameWorld m => ConduitT () Int m ()
naiveAI = forever do
  paddleX <- use (gwPaddlePos._x)
  ballX <- use (gwBallPos._x)
  C.yield (traceShowId $ signum (ballX - paddleX))

playTillEnd :: Code -> Int
playTillEnd code = let
  interp = execVM defaultOpcodeMap
  initVM = newVM code
  initWorld = GameWorld Map.empty (V2 0 0) (V2 0 0) 0

  screenAndScore :: Monad m => ConduitT (V2 Int, Int) Void (StateT GameWorld m) Int
  screenAndScore = C.await >>= \case
    Nothing -> use gwScore
    Just (V2 (-1) 0, score) -> do
      gwScore .= score
      screenAndScore
    Just (pos, tileId) -> do
      let tile = tileFromInt tileId
      gwGrid.at pos .= tile
      case tile of 
        Just Ball -> gwBallPos .= pos
        Just Paddle -> gwPaddlePos .= pos
        _ -> pure ()
      done <- isNothing <$> preuse (gwGrid.folded.filtered (== Block))
      screenAndScore
    
  result = C.runConduitPure . C.evalStateC (initVM, initWorld)
    $  C.transPipe (zoom _2) naiveAI
    .| C.transPipe (zoom _1) interp
    .| chunker
    .| C.transPipe (zoom _2) screenAndScore
  in result