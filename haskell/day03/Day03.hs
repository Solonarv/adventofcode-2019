module Day03 where

import Data.Foldable
import Data.Ord
import Data.Traversable
import Data.List.NonEmpty (NonEmpty)
-- import qualified Data.List.NonEmpty as NE
import Data.Monoid (Ap(..))

import qualified Control.Monad.Combinators.NonEmpty as NE
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import AOC.Solution
import ParsingPrelude
import Util

solution :: Solution (NonEmpty PathSpec) Int
solution = Solution
  { decodeInput = pPath `NE.sepBy1` eol
  , parts = "ab"
  , solvePart = \case
    'a' -> fmap pointVal . intersectionClosestToOrigin
    'b' -> intersectionClosestAlongWires
    _ -> const Nothing
  , showResult = const show
  , tests =
    [ "R8,U5,L5,D3\nU7,R6,D4,L4" :=>
      [ ('a', "6")
      , ('b', "30")
      ]
    , "R75,D30,R83,U83,L12,D49,R71,U7,L72\n\
      \U62,R66,U55,R34,D71,R55,D58,R83" :=>
      [ ('a', "159")
      , ('b', "610")
      ]
    , "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51\n\
      \U98,R91,D20,R16,D67,R40,U7,R15,U6,R7" :=>
      [ ('a', "135")
      , ('b', "410")
      ]
    ]
  }
  where
    pPath = pSegment `sepBy` char ','
    pSegment = Segment <$> pDirection <*> decimal
    pDirection = asum
      [ R <$ char 'R'
      , L <$ char 'L'
      , U <$ char 'U'
      , D <$ char 'D'
      ]

type PathSpec = [Segment]

data Segment = Segment !Direction !Int
  deriving (Eq, Ord, Show)

data Direction = R | L | U | D
  deriving (Eq, Ord, Show)

data Point a = Pt { px, py :: !a }
  deriving (Eq, Functor)
  deriving Num via (Ap Point a)

pointVal :: Num a => Point a -> a
pointVal (Pt x y) = abs x + abs y

instance Applicative Point where
  pure a = Pt a a
  Pt fx fy <*> Pt x y = Pt (fx x) (fy y)

instance (Ord a, Num a) => Ord (Point a) where
  compare = comparing pointVal <> comparing px <> comparing py

instance Show a => Show (Point a) where
  showsPrec p (Pt x y) = showParen (p>10)
    $ showString "Pt "
    . showsPrec 10 x
    . showString " "
    . showsPrec 10 y

pathPoints :: PathSpec -> [(Point Int, Int)]
pathPoints = concat . snd . mapAccumL go (Pt 0 0, 0)
  where
    go (pos, l) (Segment R d) = ((pos + Pt d 0, l+d), [(pos + Pt x 0, x+l) | x <- [1..d]])
    go (pos, l) (Segment L d) = ((pos - Pt d 0, l+d), [(pos - Pt x 0, x+l) | x <- [1..d]])
    go (pos, l) (Segment U d) = ((pos + Pt 0 d, l+d), [(pos + Pt 0 y, y+l) | y <- [1..d]])
    go (pos, l) (Segment D d) = ((pos - Pt 0 d, l+d), [(pos - Pt 0 y, y+l) | y <- [1..d]])

intersectionClosestToOrigin :: NonEmpty PathSpec -> Maybe (Point Int)
intersectionClosestToOrigin = Set.lookupMin . foldr1 Set.intersection . fmap (Set.fromList . map fst . pathPoints)

intersectionClosestAlongWires :: NonEmpty PathSpec -> Maybe Int
intersectionClosestAlongWires spec = let

  intersections :: Map (Point Int) Int
  intersections = foldr1 (Map.intersectionWith (+)) . fmap (Map.fromList . pathPoints) $ spec

  in collapseMapWith (flip const) min intersections

showPoints :: Set (Point Int) -> [String]
showPoints pts = [[if Pt x y `Set.member` (Set.insert 0 pts) then '#' else '.' | x <- [-d..d]] | y <- [-d..d]]
  where d = maybe 0 pointVal (Set.lookupMax pts)