module Day03 where

-- import Control.Applicative
import Data.Foldable
import Data.Ord
import Data.Traversable
import Data.List.NonEmpty (NonEmpty)
-- import qualified Data.List.NonEmpty as NE
import Data.Monoid (Ap(..))

import qualified Control.Monad.Combinators.NonEmpty as NE
import Data.Set (Set)
import qualified Data.Set as Set

import AOC.Solution
import ParsingPrelude
-- import Util

solution :: Solution (NonEmpty PathSpec) Int
solution = Solution
  { decodeInput = pPath `NE.sepBy1` eol
  , parts = "ab"
  , solvePart = \case
    'a' -> fmap pointVal . Set.lookupMin . foldr1 Set.intersection . fmap pathPoints
    _ -> const Nothing
  , showResult = const show
  , tests =
    [ "R8,U5,L5,D3\nU7,R6,D4,L4" :=>
      [ ('a', "6")
      ]
    , unlines
      [ "R75,D30,R83,U83,L12,D49,R71,U7,L72"
      , "U62,R66,U55,R34,D71,R55,D58,R83"
      ] :=>
      [ ('a', "159")
      ]
    , unlines
      [ "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51"
      , "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"
      ] :=>
      [ ('a', "135")
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

data Segment = Segment Direction Int

data Direction = R | L | U | D

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

pathPoints :: PathSpec -> Set (Point Int)
pathPoints = Set.fromList . concat . snd . mapAccumR go (Pt 0 0)
  where
    go pos (Segment R d) = (pos + Pt d 0, [pos + Pt x 0 | x <- [1..d]])
    go pos (Segment L d) = (pos - Pt d 0, [pos - Pt x 0 | x <- [1..d]])
    go pos (Segment U d) = (pos + Pt 0 d, [pos - Pt 0 y | y <- [1..d]])
    go pos (Segment D d) = (pos - Pt 0 d, [pos - Pt 0 y | y <- [1..d]])
