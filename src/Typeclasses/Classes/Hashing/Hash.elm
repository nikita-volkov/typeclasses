module Typeclasses.Classes.Hashing.Hash exposing (..)

import Bitwise
import Bytes.Encode
import Bytes.Decode
import Bytes


intWithSalt : Int -> Int -> Int
intWithSalt a b = Bitwise.xor (a * 16777619) b

char : Char -> Int
char = Char.toCode

charWithSalt : Int -> Char -> Int
charWithSalt salt x = intWithSalt salt (Char.toCode x)

bool : Bool -> Int
bool x = if x then 1 else 0

{-|
Leverage existing encoder to _cast_ a float64 into an unsigned
int32 on bytes.

Inspired by a smart workaround here:
https://github.com/elm-toulouse/float16/blob/1.0.1/src/Bytes/Floating/Encode.elm#L127-L135
-}
float : Float -> Int
float =
  Bytes.Encode.float64 Bytes.LE >>
  Bytes.Encode.encode >>
  Bytes.Decode.decode (Bytes.Decode.unsignedInt32 Bytes.LE) >>
  Maybe.withDefault 0
