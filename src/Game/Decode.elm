module Game.Decode where


import Game.Model exposing (..)
import Json.Decode exposing (Decoder, (:=))
import Dict exposing (Dict)
import String
import Result.Extra

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


mkTile : Letter -> Result String Tile
mkTile l =
    Dict.get (toString l) points
        `Maybe.andThen` (\pts -> Just (Tile l pts))
        |> Result.fromMaybe "Tile creation error"



tile : Decoder Tile
tile =
    Json.Decode.customDecoder
        letter mkTile


-- needed to parse the unusual format
tileString : String -> Result String (List Tile)
tileString str =
    String.toList str
        |> List.map String.fromChar
        |> List.map
            (\x -> parseLetter x
                `Result.andThen` mkTile)
        |> Result.Extra.combine


tiles : Decoder (List Tile)
tiles =
    Json.Decode.customDecoder
        Json.Decode.string tileString


rack : Decoder Rack
rack =
    Json.Decode.map Rack tiles


bag : Decoder Bag
bag = Json.Decode.map Bag tiles


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


point : Decoder (Int,Int)
point =
    Json.Decode.tuple2 (,) Json.Decode.int Json.Decode.int


square : Decoder Square
square =
    Json.Decode.object3 Square
        ("tile" := Json.Decode.maybe tile )
        ("bonus" := bonus)
        ("squarePos" := point)


board : Decoder Board
board =
    Json.Decode.list (Json.Decode.tuple2 (,) point tile)
        |> Json.Decode.map Board


playerType : Decoder PlayerType
playerType =
    let parsePlayerType str =
            case str of
                "Human" -> Ok Human
                "AI" -> Ok AI
                _ -> Err ("PlayerType parse failure: " ++ str)
    in Json.Decode.customDecoder
        Json.Decode.string parsePlayerType


player : Decoder Player
player =
    Json.Decode.object5 Player
        ("playerType" := playerType)
        ("playerName" := Json.Decode.string )
        ("playerRack" := rack)
        ("playerScore" := Json.Decode.int )
        ("playerId" := Json.Decode.int)


tilePut : Decoder TilePut
tilePut =
    (\(pt, tile, score) ->
        if score == 0
            then BlankTilePut (tile.tileLetter) pt
            else LetterTilePut tile pt)
    `Json.Decode.map`
        ( Json.Decode.tuple3 (,,)
            point tile Json.Decode.int
        )


wordPut : Decoder WordPut
wordPut =
    Json.Decode.object1 WordPut
        ("wordPutTiles" := Json.Decode.list tilePut)


turn : Decoder Turn
turn =
     Json.Decode.object5 Turn
        ("playerId" := Json.Decode.int)
        ("tilesPlayed" := wordPut)
        ("points" := Json.Decode.int)
        ("rackRemainder" := rack)
        ("tilesTakenFromBag" := tiles)


game : Decoder Game
game =
    Json.Decode.object4 Game
        ("gamePlayers" := Json.Decode.list player)
        ("gameBoard" := board)
        ("gameBag" := bag)
        ("gameTurns" := Json.Decode.list turn)