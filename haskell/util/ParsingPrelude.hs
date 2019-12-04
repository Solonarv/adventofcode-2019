module ParsingPrelude (module X, module Text.Megaparsec.Char.Lexer, Parser) where

import Data.Void

import Text.Megaparsec as X
import Text.Megaparsec.Char as X
import Text.Megaparsec.Char.Lexer (decimal, binary, octal, hexadecimal, scientific, float, signed)

type Parser = Parsec Void String
