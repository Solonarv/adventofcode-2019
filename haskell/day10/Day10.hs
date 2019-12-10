module Day10 where

import Data.Foldable
import Data.Functor
import Data.List

import Data.Map (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified Data.Vector.Unboxed as UVector
import Linear.V2

import AOC.Solution
import ParsingPrelude
import RatAngle
import Util

solution :: Solution [V2 Int] Int
solution = Solution
  { decodeInput = toPoints . Vector.fromList <$> (UVector.fromList <$> some dot) `sepBy` eol
  , parts = "ab"
  , solvePart = \case
    'a' -> fmap fst . bestMonitoringStation
    'b' -> fmap (\(V2 x y) -> 100*x+y) . nthVaporized 200
    _ -> const Nothing
  , showResult = const show
  , tests =
    [ "......#.#.\n\
      \#..#.#....\n\
      \..#######.\n\
      \.#.#.###..\n\
      \.#..#.....\n\
      \..#....#.#\n\
      \#..#....#.\n\
      \.##.#..###\n\
      \##...#..#.\n\
      \.#....####" :=>
      [ ('a', "33")
      ]
    , "#.#...#.#.\n\
      \.###....#.\n\
      \.#....#...\n\
      \##.#.#.#.#\n\
      \....#.#.#.\n\
      \.##..###.#\n\
      \..#...##..\n\
      \..##....##\n\
      \......#...\n\
      \.####.###." :=>
      [ ('a', "35")
      ]
    , ".#..#..###\n\
      \####.###.#\n\
      \....###.#.\n\
      \..###.##.#\n\
      \##.##.#.#.\n\
      \....###..#\n\
      \..#.#..#.#\n\
      \#..#.#.###\n\
      \.##...##.#\n\
      \.....#.#.." :=>
      [ ('a', "41")
      ]
    , ".#..##.###...#######\n\
      \##.############..##.\n\
      \.#.######.########.#\n\
      \.###.#######.####.#.\n\
      \#####.##.#.##.###.##\n\
      \..#####..#.#########\n\
      \####################\n\
      \#.####....###.#.#.##\n\
      \##.#################\n\
      \#####.##.###..####..\n\
      \..######..##.#######\n\
      \####.##.####...##..#\n\
      \.#####..#.######.###\n\
      \##...#.##########...\n\
      \#.##########.#######\n\
      \.####.#.###.###.#.##\n\
      \....##.##.###..#####\n\
      \.#.#.###########.###\n\
      \#.#.#.#####.####.###\n\
      \###.##.####.##.#..##" :=>
      [ ('a', "210")
      , ('b', "802")
      ]
    ]
  }
  where dot = asum [False <$ char '.', True <$ char '#']

toPoints :: Vector (UVector.Vector Bool) -> [V2 Int]
toPoints grid
  = Vector.ifoldr
    (\i row dxss ->
      UVector.ifoldr (\j p dxs ->
        if p
          then (V2 j i:) . dxs
          else dxs
      )
      dxss
      row
    )
    id grid []

bestMonitoringStation :: [V2 Int] -> Maybe (Int, V2 Int)
bestMonitoringStation asteroids = safely (maximumOn fst)
  [ (length $ Set.fromList
      [ atan2r (other - self)
      | other <- asteroids
      , other /= self
      ]
    , self
    )
  | self <- asteroids
  ]

nthVaporized :: Int -> [V2 Int] -> Maybe (V2 Int)
nthVaporized n asteroids = do
  (_, station) <- bestMonitoringStation asteroids
  let targets :: Map (RatAngle Int) ([V2 Int])
      targets = Map.fromListWith
        (++)
        [ (atan2r (perp dv), [v])
        | v <- asteroids
        , v /= station
        , let dv = v - station
        ]
        <&> sortOn (sum . fmap abs . subtract station)

      nth_rel = snd <$> multimapAt (n-1) targets
  nth_rel