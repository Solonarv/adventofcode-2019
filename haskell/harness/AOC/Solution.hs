module AOC.Solution where

import Data.Void (Void)

import Text.Megaparsec (Parsec)

type Part = Char

data Solution a b = Solution
  { decodeInput :: Parsec Void String a
  , parts       :: [Part]
  , solvePart   :: Part -> a -> Maybe b
  , showResult  :: Part -> b -> String
  , tests       :: [Test]
  }

data Test = String :=> [(Part, String)]