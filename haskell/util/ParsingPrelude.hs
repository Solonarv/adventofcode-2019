module ParsingPrelude (module X, module Text.Megaparsec.Char.Lexer) where

import Text.Megaparsec as X
import Text.Megaparsec.Char as X
import Text.Megaparsec.Char.Lexer (decimal, binary, octal, hexadecimal, scientific, float, signed)
