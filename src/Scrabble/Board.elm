module Scrabble.Board where

import Dict exposing (Dict)
import Json.Decode exposing (Decoder, (:=))
import Json.Encode exposing (Value)
import Result.Extra
import String

type Letter =
  A | B | C | D | E | F | G | H | I | J | K | L | M |
  N | O | P | Q | R | S | T | U | V | W | X | Y | Z | Blank

encodeLetter : Letter -> Value
encodeLetter = Json.Encode.string << toString


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


type alias Score = Int

type alias Points = Int

-- wasn't the case before, but now permitted by blank encoding
showLetter : Letter -> String
showLetter = toString

letterPoints : Letter -> Maybe Points
letterPoints l =
    Dict.get (showLetter l) points


-- Has to be string because of constraints on Dict keys,
-- first time lack of typeclasses has bit me!
points : Dict String Points
points = Dict.fromList
            [ ("A",1), ("B",3), ("C",3),("D",2),("E",1)
            , ("F",4), ("G",2), ("H",4),("I",1),("J",8)
            , ("K",5), ("L",1), ("M",3),("N",1),("O",1)
            , ("P",3), ("Q",10),("R",1),("S",1),("T",1)
            , ("U",1), ("V",4), ("W",4),("X",8),("Y",4)
            , ("Z",10), ("Blank", 0)
            ]


type alias Tile =
    { tileLetter : Letter, score : Int }

-- (<<) = (.)
encodeTile : Tile -> Value
encodeTile = encodeLetter << .tileLetter



tile : Decoder Tile
tile =
    Json.Decode.customDecoder
        letter mkTile


mkTile : Letter -> Result String Tile
mkTile l =
    letterPoints l
        `Maybe.andThen` (\pts -> Just (Tile l pts))
        |> Result.fromMaybe "tile creation error"

encodeTiles : List Tile -> Value
encodeTiles tiles =
    tiles
        |> List.map (toString << .tileLetter)
        |> String.concat
        |> Json.Encode.string

tiles : Decoder (List Tile)
tiles =
    Json.Decode.customDecoder
        Json.Decode.string tileString

-- needed to parse the unusual format
tileString : String -> Result String (List Tile)
tileString str =
    String.toList str
        |> List.map String.fromChar
        |> List.map
            (\x -> parseLetter x
                `Result.andThen` mkTile)
        |> Result.Extra.combine


type alias Rack =
    { rackTiles : List Tile }

encodeRack : Rack -> Value
encodeRack = encodeTiles << .rackTiles

rack : Decoder Rack
rack = Json.Decode.map Rack tiles


type alias Bag =
    { bagTiles : List Tile }


encodeBag : Bag -> Value
encodeBag = encodeTiles << .bagTiles


bag : Decoder Bag
bag = Json.Decode.map Bag tiles


type Bonus = W3 | W2 | L3 | L2 | Star | NoBonus

encodeBonus : Bonus -> Value
encodeBonus = Json.Encode.string << toString

bonus : Decoder Bonus
bonus =
    let parseBonus b =
            case b of
                "W3" -> Ok W3
                "W2" -> Ok W2
                "L3" -> Ok L3
                "L2" -> Ok L2
                "Star" -> Ok Star
                "NoBonus" -> Ok NoBonus
                _ -> Err ("bonus parse error: " ++ b)
    in Json.Decode.customDecoder
        Json.Decode.string parseBonus


type alias Point = (Int,Int)


encodePoint : Point -> Value
encodePoint (x,y) =
    [x,y]
        |> List.map Json.Encode.int
        |> Json.Encode.list


point : Decoder (Int,Int)
point =
    Json.Decode.tuple2 (,) Json.Decode.int Json.Decode.int


type alias Square =
    { tile : Maybe Tile
    , bonus : Bonus
    , squarePos : Point
    }



encodeMaybe : (a -> Value) -> Maybe a -> Value
encodeMaybe f m =
    Maybe.withDefault Json.Encode.null (Maybe.map f m)


encodeSquare : Square -> Value
encodeSquare s =
    Json.Encode.object
        [ ("squarePos", encodePoint s.squarePos)
        , ("tile", encodeMaybe encodeTile s.tile)
        , ("bonus", encodeBonus s.bonus)
        ]


square : Decoder Square
square =
    Json.Decode.object3 Square
        ("tile" := Json.Decode.maybe tile )
        ("bonus" := bonus)
        ("squarePos" := point)


{-TODO change `Tile` to `Square` and use the position to infer
  the bonus at that square. Will have to do something similar
  to `Tile`, keeping a lookup table of bonuses and retreieving
  them when the position gets parsed
-}
type alias Board =
    { contents : List (Point, Tile) }


encodeBoard : Board -> Value
encodeBoard b =
    let encodeEntry (p,t) =
            Json.Encode.list [encodePoint p, encodeTile t]
    in Json.Encode.object
        [ ("contents", Json.Encode.list
                        (List.map encodeEntry b.contents)
          )
        ]


board : Decoder Board
board =
    Json.Decode.list (Json.Decode.tuple2 (,) point tile)
        |> Json.Decode.map Board
