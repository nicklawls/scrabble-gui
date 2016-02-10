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


type TilePut
    = LetterTilePut Tile Point
    | BlankTilePut Letter Point


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


wordPut : Decoder WordPut
wordPut =
    Json.Decode.object1 WordPut
        ("wordPutTiles" := Json.Decode.list tilePut)


type alias Turn =
    { playerId : Int
    , tilesPlayed : WordPut
    , points : Points
    , rackRemainder : Rack
    , tilesTakenFromBag : List Tile
    }


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


game : Decoder Game
game =
    Json.Decode.object4 Game
        ("gamePlayers" := Json.Decode.list Player.player)
        ("gameBoard" := Board.board)
        ("gameBag" := Board.bag)
        ("gameTurns" := Json.Decode.list turn)

init : Game
init = Game [] (Board []) (Bag []) []
