module Scrabble.Game where

import Scrabble.Player exposing (Player)
import Scrabble.Board
    exposing (Rack, Tile, Board, Bag, Point, Letter, Points)

-- [[7,6], "A", 4]
type TilePut
    = LetterTilePut Tile Point
    | BlankTilePut Letter Point


type alias WordPut =
    { wordPutTiles : List TilePut }


type alias Turn =
    { playerId : Int
    , tilesPlayed : WordPut
    , points : Points
    , rackRemainder : Rack
    , tilesTakenFromBag : Tile
    }


type alias Game =
    { gamePlayers : List Player
    , gameBoard : Board
    , gameBag : Bag
    , gameTurns : List Turn
    }
