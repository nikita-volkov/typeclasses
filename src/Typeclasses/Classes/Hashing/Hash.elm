module Typeclasses.Classes.Hashing.Hash exposing (..)

import Bitwise

intWithSalt : Int -> Int -> Int
intWithSalt a b = Bitwise.xor (a * 16777619) b

char : Char -> Int
char = Char.toCode

charWithSalt : Int -> Char -> Int
charWithSalt salt x = intWithSalt salt (Char.toCode x)

bool : Bool -> Int
bool x = if x then 1 else 0
