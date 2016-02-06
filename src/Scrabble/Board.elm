module Scrabble.Board where

import Dict exposing (Dict)
import Json.Decode exposing (Decoder, (:=))
import Result.Extra
import String

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
        "_" -> Ok Blank
        _   -> Err "letter parse error"


letter : Decoder Letter
letter = Json.Decode.customDecoder
            Json.Decode.string parseLetter


type alias Score = Int

type alias Points = Int


showLetter : Letter -> String
showLetter l =
    case l of
        Blank -> "_"
        _ -> toString l

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
            , ("Z",10), ("_", 0)
            ]


type alias Tile =
    { tileLetter : Letter, score : Int }


tile : Decoder Tile
tile =
    Json.Decode.customDecoder
        letter mkTile


mkTile : Letter -> Result String Tile
mkTile l =
    letterPoints l
        `Maybe.andThen`
            (\pts -> Just {tileLetter = l, score = pts})
        |> Result.fromMaybe "tile creation error"


type alias Rack =
    { rackTiles : List Tile }


rack : Decoder Rack
rack = (\x -> { rackTiles = x } ) `Json.Decode.map`
        Json.Decode.customDecoder Json.Decode.string tileString


type alias Bag =
    { bagTiles : List Tile }


bag : Decoder Bag
bag = (\x -> { bagTiles = x } ) `Json.Decode.map`
        Json.Decode.customDecoder Json.Decode.string tileString


-- needed to parse the unusual format
tileString : String -> Result String (List Tile)
tileString str =
    String.toList str
        |> List.map String.fromChar
        |> List.map
            ( \x -> parseLetter x
                `Result.andThen` mkTile)
        |> Result.Extra.combine




type Bonus = W3 | W2 | L3 | L2 | Star | NoBonus


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
                _ -> Err "bonus parse error"
    in Json.Decode.customDecoder
        Json.Decode.string parseBonus

type alias Point = (Int,Int)


point : Decoder (Int,Int)
point =
    Json.Decode.tuple2 (,) Json.Decode.int Json.Decode.int


type alias Square =
    { tile : Maybe Tile
    , bonus : Bonus
    , squarePos : Point
    }


square : Decoder Square
square =
    Json.Decode.object3
        (\t b sp -> { tile = t, bonus = b, squarePos = sp })
        ("tile" := Json.Decode.maybe tile )
        ("bonus" := bonus)
        ("squarePos" := point)

-- in the haskell code, Board is {contents :: Array (Int,Int) Tile}
-- not sure if losing info
type alias Board =
    { contents : List ((Int,Int), Letter, Score) }

board : Decoder Board
board =
    (\lst -> { contents = lst } ) `Json.Decode.map`
        Json.Decode.list (Json.Decode.tuple3 (,,) point letter Json.Decode.int )
