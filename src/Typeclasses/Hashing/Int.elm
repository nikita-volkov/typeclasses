module Typeclasses.Hashing.Int exposing (..)

import Bitwise

combine : Int -> Int -> Int
combine a b = Bitwise.xor (a * 16777619) b
