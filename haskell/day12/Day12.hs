{-# LANGUAGE TemplateHaskell #-}
module Day12 where

import Data.Foldable
import Text.Printf (printf)

import Control.Lens hiding (parts)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified Data.Vector.Unboxed as UVector
import Linear.V3

import AOC.Solution
import ParsingPrelude
import Util

type Axis = UVector.Vector (Int, Int)

data World = World
  { _worldX, _worldY, _worldZ :: !Axis
  }
makeLenses ''World

solution :: Solution World Int
solution = Solution
  { decodeInput = initWorld . Vector.fromList <$> (coords `sepBy` eol)
  , parts = "ab"
  , solvePart = \case
    'a' -> Just . worldEnergy . funcpow 1000 stepSimWorld
    'b' -> Just . stepsTillRepeat
    _ -> const Nothing
  , showResult = const show
  , tests = []
  }
  where
    int = signed space decimal
    coords = do
      x <- string "<x=" *> int
      y <- string ", y=" *> int
      z <- string ", z=" *> int
      V3 x y z <$ string ">"

initWorld :: Vector (V3 Int) -> World
initWorld positions = let
  len = length positions
  _worldX = UVector.generate len (\i -> (positions ^?! ix i._x, 0))
  _worldY = UVector.generate len (\i -> (positions ^?! ix i._y, 0))
  _worldZ = UVector.generate len (\i -> (positions ^?! ix i._z, 0))
  in World{_worldX, _worldY, _worldZ}

showWorld :: World -> [String]
showWorld (World xs ys zs) = zipWith3
  (\(rx, vx) (ry, vy) (rz, vz) ->
    printf "pos=<x=%v, y=%v, z=%v>, vel=<x=%v, y=%v, z=%v>" rx ry rz vx vy vz
    )
  (UVector.toList xs)
  (UVector.toList ys)
  (UVector.toList zs)

stepSimAx :: Axis -> Axis
stepSimAx = applyVel . applyGrav
  where
    applyGrav ax = flip UVector.map ax \(pos,vel) -> let
      vel' = UVector.foldl' (\acc (other,_) -> acc + signum (other - pos)) vel ax
      in (pos, vel')
    applyVel = UVector.map (\m -> m & _1 +~ (m^._2))

stepSimWorld :: World -> World
stepSimWorld = (worldX %~ stepSimAx) . (worldY %~ stepSimAx) . (worldZ %~ stepSimAx)

moonEnergy :: (Int, Int) -> (Int, Int) -> (Int, Int) -> Int
moonEnergy (x,vx) (y,vy) (z,vz) = kin * pot
  where
    pot = sum' . fmap abs $ [x,y,z]
    kin = sum' . fmap abs $ [vx,vy,vz]

worldEnergy :: World -> Int
worldEnergy (World xs ys zs) = UVector.sum
  $ UVector.zipWith3
    moonEnergy
    xs ys zs

stepsTillRepeat :: World -> Int
stepsTillRepeat (World xs ys zs) = foldl' lcm 1 . map stepsTillRepeatAx $ [xs, ys, zs]

stepsTillRepeatAx :: Axis -> Int
stepsTillRepeatAx ax = go 1 (stepSimAx ax)
  where
    go !n ax'
      | ax == ax' = n
      | otherwise = go (n+1) (stepSimAx ax')