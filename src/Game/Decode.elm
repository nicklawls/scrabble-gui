module Game.Decode where


import Game.Model exposing (..)
import Json.Decode exposing (Decoder, (:=))
import Dict exposing (Dict)
import String
import Result.Extra
import List.Extra
import Letter exposing (Letter, letter, parseLetter)
--import Json.Decode.Extra as Decode


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


-- flip to give preference to 2nd argment
updateMany : Dict comparable v -> Dict comparable v -> Dict comparable v
updateMany = flip Dict.union


board : Decoder Board
board =
    Json.Decode.list (Json.Decode.tuple2 (,) point tile)
        |> Json.Decode.map (putTiles defaultBoard)


putTiles : Board -> List (Point,Tile) -> Board
putTiles {contents} pts =
    let tileToSquare (p,t) =
            ( p
            , Dict.get p contents
                |> Maybe.withDefault (Square Nothing NoBonus (0,0))
                |> \s -> {s | tile = Just t}
            )
    in
        Board <| updateMany contents ( Dict.fromList <| List.map tileToSquare pts)


defaultBoard : Board
defaultBoard =
    let points =
            [0..14] `List.Extra.andThen` \x ->
            [0..14] `List.Extra.andThen` \y ->
            [(x,y)]

        o = NoBonus

        s = Star

        bonuses = List.concat
          [ [W3,  o,  o, L2,  o,  o,  o, W3,  o,  o,  o, L2,  o,  o, W3]
          , [ o, W2,  o,  o,  o, L3,  o,  o,  o, L3,  o,  o,  o, W2,  o]
          , [ o,  o, W2,  o,  o,  o, L2,  o, L2,  o,  o,  o, W2,  o,  o]
          , [L2,  o,  o, W2,  o,  o,  o, L2,  o,  o,  o, W2,  o,  o, L2]
          , [ o,  o,  o,  o, W2,  o,  o,  o,  o,  o, W2,  o,  o,  o,  o]
          , [ o, L3,  o,  o,  o, L3,  o,  o,  o, L3,  o,  o,  o, L3,  o]
          , [ o,  o, L2,  o,  o,  o, L2,  o, L2,  o,  o,  o, L2,  o,  o]
          , [W3,  o,  o, L2,  o,  o,  o,  s,  o,  o,  o, L2,  o,  o, W3]
          , [ o,  o, L2,  o,  o,  o, L2,  o, L2,  o,  o,  o, L2,  o,  o]
          , [ o, L3,  o,  o,  o, L3,  o,  o,  o, L3,  o,  o,  o, L3,  o]
          , [ o,  o,  o,  o, W2,  o,  o,  o,  o,  o, W2,  o,  o,  o,  o]
          , [L2,  o,  o, W2,  o,  o,  o, L2,  o,  o,  o, W2,  o,  o, L2]
          , [ o,  o, W2,  o,  o,  o, L2,  o, L2,  o,  o,  o, W2,  o,  o]
          , [ o, W2,  o,  o,  o, L3,  o,  o,  o, L3,  o,  o,  o, W2,  o]
          , [W3,  o,  o, L2,  o,  o,  o, W3,  o,  o,  o, L2,  o,  o, W3]
          ]

    in List.Extra.zip points bonuses
        |> List.map (\(point,bonus) -> (point, Square Nothing bonus point) )
        |> Dict.fromList
        |> Board


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

decodeGame : String -> Result String Game
decodeGame =
    Json.Decode.decodeString game