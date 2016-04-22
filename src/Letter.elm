module Letter where

import Json.Decode exposing (Decoder)
import Json.Encode exposing (Value)


type Letter =
  A | B | C | D | E | F | G | H | I | J | K | L | M |
  N | O | P | Q | R | S | T | U | V | W | X | Y | Z | Blank


parseLetter : String -> Result String Letter
parseLetter str =
  case str of
      "A" -> Ok A
      "B" -> Ok B
      "C" -> Ok C
      "D" -> Ok D
      "E" -> Ok E
      "F" -> Ok F
      "G" -> Ok G
      "H" -> Ok H
      "I" -> Ok I
      "J" -> Ok J
      "K" -> Ok K
      "L" -> Ok L
      "M" -> Ok M
      "N" -> Ok N
      "O" -> Ok O
      "P" -> Ok P
      "Q" -> Ok Q
      "R" -> Ok R
      "S" -> Ok S
      "T" -> Ok T
      "U" -> Ok U
      "V" -> Ok V
      "W" -> Ok W
      "X" -> Ok X
      "Y" -> Ok Y
      "Z" -> Ok Z
      "Blank" -> Ok Blank
      "_"     -> Ok Blank -- here to help tile along
      _   -> Err ("letter parse error: " ++ str)


letter : Decoder Letter
letter = Json.Decode.customDecoder
          Json.Decode.string parseLetter

letters : List Letter
letters =
    [ A, B, C, D, E, F, G
    , H, I, J, K, L, M, N, O, P
    , Q, R, S, T, U, V, W, X, Y, Z
    ]

letterString : Letter -> String
letterString l =
    if l == Blank
        then "_"
        else toString l

encodeLetter : Letter -> Value
encodeLetter = Json.Encode.string << letterString