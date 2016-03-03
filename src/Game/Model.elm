module Game.Model where

import Dict exposing (Dict)


type Letter =
  A | B | C | D | E | F | G | H | I | J | K | L | M |
  N | O | P | Q | R | S | T | U | V | W | X | Y | Z | Blank


type alias Score = Int


type alias Points = Int


type alias Tile =
    { tileLetter : Letter, score : Int }


type alias Rack =
    { rackTiles : List Tile }


type alias Bag =
    { bagTiles : List Tile }


type Bonus = W3 | W2 | L3 | L2 | Star | NoBonus


type alias Point = (Int,Int)


type alias Square =
    { tile : Maybe Tile
    , bonus : Bonus
    , squarePos : Point
    }


type alias Board =
    { contents : Dict Point Square }


type PlayerType = Human | AI


type alias Name = String


type alias Player =
    { playerType : PlayerType
    , playerName : Name
    , playerRack : Rack
    , playerScore : Score
    , playerId : Int
    }


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
    , tilesTakenFromBag : List Tile
    }


type alias Game =
    { gamePlayers : List Player
    , gameBoard : Board
    , gameBag : Bag
    , gameTurns : List Turn
    }


type PlayerId
    = Unassigned
    | Zero
    | One


type alias TileOffset = (Float,Float)


type alias Model =
    { game : Game
    , tileOffsets : Dict Point TileOffset 
    }


initialModel : Model
initialModel = Model (Game [] (Board Dict.empty) (Bag []) []) Dict.empty