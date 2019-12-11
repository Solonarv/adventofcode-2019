{-# LANGUAGE TemplateHaskell #-}
module Day11 where

import Control.Applicative
import Data.Foldable
import Data.Maybe
import Data.Void

import Control.Lens hiding (parts)
import Control.Monad.State.Strict
import Data.Conduit (ConduitT, (.|))
import qualified Data.Conduit as C
import Data.Conduit.Lift as C
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Linear.V2

import AOC.Solution
import Intcode

type Color = Int

data PhysicalWorld = PhysicalWorld
  { _panels :: Map (V2 Int) Color
  , _currentPos :: V2 Int
  , _heading :: V2 Int
  }
  deriving (Eq, Ord, Show)

makeLenses ''PhysicalWorld

solution :: Solution Code (Map (V2 Int) Color)
solution = Solution
  { decodeInput = parseIntcode
  , parts = "ab"
  , solvePart = \case
    'a' -> Just . paintedSquares Set.empty
    'b' -> Just . paintedSquares (Set.singleton (V2 0 0))
    _ -> const Nothing
  , showResult = \case
    'a' -> show . length
    'b' -> drawSquares
    _ -> show
  , tests =
    [ "99" :=> [('a', "0")]
    , "3,1,104,1,104,1,99" :=> [('a', "1")]
    , "3,600,104,1,104,1,3,600,104,1,104,0,3,600,104,1,104,1,99" :=> [('a', "3")]
    ]
  }

initialWorld :: Set (V2 Int) -> PhysicalWorld
initialWorld whites = PhysicalWorld (Map.fromSet (const 1) whites) (V2 0 0) (V2 1 0)

sensor :: ConduitT () Int (State (PhysicalWorld, VMState)) ()
sensor = C.transPipe (zoom _1) $ sensor'
  where
    sensor' :: ConduitT () Int (State PhysicalWorld) ()
    sensor' = forever do
      pos <- use currentPos
      color <- use (panels.at pos)
      C.yield $ fromMaybe 0 color

actuators :: ConduitT Int Void (State (PhysicalWorld, VMState)) ()
actuators = C.transPipe (zoom _1) actuators'
 where
  actuators' :: ConduitT Int Void (State PhysicalWorld) ()
  actuators' = do
    C.await >>= traverse_ \color ->
      C.await >>= traverse_ \turnDir -> do
        pos <- use currentPos
        panels.at pos .= Just color
        dir <- heading <%= case turnDir of
          0 -> perp
          1 -> negate . perp
          i -> error $ "invalid turn code: " <> show i
        currentPos += dir
        actuators'

interp :: ConduitT Int Int (State (PhysicalWorld, VMState)) ()
interp = C.transPipe (zoom _2) (execVM defaultOpcodeMap)

paintedSquares :: Set (V2 Int) -> Code -> Map (V2 Int) Color
paintedSquares whites code = let
  initialState = (initialWorld whites, newVM code)
  finalState = C.runConduitPure $ C.execStateC initialState (sensor .| interp .| actuators)
  in finalState ^. _1.panels


drawSquares :: Map (V2 Int) Color -> String
drawSquares grid = let
  (V2 minX minY, V2 maxX maxY) = Map.foldlWithKey'
    (\(l,h) v c -> if c > 0
      then (liftA2 min l v, liftA2 max h v)
      else (l,h))
    (V2 0 0, V2 0 0)
    grid
  in "drawn image:\n" ++ unlines
      [ [ if V2 x y `Map.lookup` grid == Just 1
            then '#'
            else ' '
        | y <- reverse [minY .. maxY]
        ]
      | x <- reverse [minX .. maxX]
      ]
