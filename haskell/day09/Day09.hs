module Day09 where

import AOC.Solution
import Intcode

solution :: Solution Code [Int]
solution = Solution
  { decodeInput = parseIntcode
  , parts = "ab"
  , solvePart = \case
    'a' -> Just . runVMPure [1] defaultOpcodeMap
    'b' -> Just . runVMPure [2] defaultOpcodeMap
    _ -> const Nothing
  , showResult = const show
  , tests = 
    [ "109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99" :=>
      [ ('a', "[109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99]")
      ]
    , "1102,34915192,34915192,7,4,7,99,0" :=>
      [ ('a', "[1219070632396864]")]
    , "104,1125899906842624,99" :=>
      [ ('a', "[1125899906842624]")
      ]
    ]
  }