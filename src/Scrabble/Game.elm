module Scrabble.Game
    ( TilePut
    , WordPut
    , Turn
    , Game
    , init
    , game
    )
    where

import Scrabble.Player as Player exposing (Player)
import Scrabble.Board as Board
    exposing (Rack, Tile, Board, Bag, Point, Letter, Points)
import Json.Decode exposing (Decoder, (:=))
import Json.Encode exposing (Value)


type TilePut
    = LetterTilePut Tile Point
    | BlankTilePut Letter Point


encodeTilePut : TilePut -> Value
encodeTilePut tp =
    case tp of
        LetterTilePut t p ->
            Json.Encode.list
                [ Board.encodePoint p
                , Board.encodeTile t
                , Json.Encode.int t.score
                ]
        BlankTilePut l p ->
            Json.Encode.list
                [ Board.encodePoint p
                , Board.encodeLetter l
                , Json.Encode.int 0
                ]


tilePut : Decoder TilePut
tilePut =
    (\(pt, tile, score) ->
        if score == 0
            then BlankTilePut (tile.tileLetter) pt
            else LetterTilePut tile pt)
    `Json.Decode.map`
        ( Json.Decode.tuple3 (,,)
            Board.point
            Board.tile
            Json.Decode.int
        )


type alias WordPut =
    { wordPutTiles : List TilePut }


encodeWordPut : WordPut -> Value
encodeWordPut {wordPutTiles} =
    wordPutTiles
        |> List.map encodeTilePut
        |> Json.Encode.list


wordPut : Decoder WordPut
wordPut =
    Json.Decode.object1 WordPut
        ("wordPutTiles" := Json.Decode.list tilePut)


type alias Turn =
    { playerId : Int
    , wordPutTiles : WordPut
    , points : Points
    , rackRemainder : Rack
    , tilesTakenFromBag : List Tile
    }


encodeTurn : Turn -> Value
encodeTurn t =
    Json.Encode.object
        [ ("playerId", Json.Encode.int t.playerId)
        , ("wordPutTiles", encodeWordPut t.wordPutTiles)
        , ("points", Json.Encode.int t.points)
        , ("rackRemainder", Board.encodeRack t.rackRemainder)
        , ("tilesTakenFromBag", Board.encodeTiles t.tilesTakenFromBag)
        ]

encodeTurns : List Turn -> Value
encodeTurns ts =
    ts
        |> List.map encodeTurn
        |> Json.Encode.list

turn : Decoder Turn
turn =
     Json.Decode.object5 Turn
        ("playerId" := Json.Decode.int)
        ("tilesPlayed" := wordPut)
        ("points" := Json.Decode.int)
        ("rackRemainder" := Board.rack)
        ("tilesTakenFromBag" := Board.tiles)


type alias Game =
    { gamePlayers : List Player
    , gameBoard : Board
    , gameBag : Bag
    , gameTurns : List Turn
    }


encodeGame : Game -> Value
encodeGame g =
    Json.Encode.object
        [ ("gamePlayers", Player.encodePlayers g.gamePlayers)
        , ("gameBoard", Board.encodeBoard g.gameBoard)
        , ("gameBag", Board.encodeBag g.gameBag)
        , ("gameTurns", encodeTurns g.gameTurns)]


game : Decoder Game
game =
    Json.Decode.object4 Game
        ("gamePlayers" := Json.Decode.list Player.player)
        ("gameBoard" := Board.board)
        ("gameBag" := Board.bag)
        ("gameTurns" := Json.Decode.list turn)


init : Game
init = Game [] (Board []) (Bag []) []
