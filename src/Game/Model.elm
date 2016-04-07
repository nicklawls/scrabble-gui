module Game.Model where

import Dict exposing (Dict)
import Set exposing (Set)
import List.Extra as List

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


type alias Offset = (Float,Float)


type TileIndex = BoardIndex Point | RackIndex Int

-- having two separate dropoff points simplifies
-- the check for a particular square having a tile

type alias Model =
    { game : Game
    , dragOffsets : Dict Point Offset
    , rackDragOffsets : Dict Int Offset
    , dropoff : Maybe Point
    , rackDropoff : Maybe Int
    , boardOrigins : Set Point
    , prevMoveValid : Bool
    }


type ClientMessage =
    Message MessageType Game WordPut

type MessageType = ValidityCheck | ActualMove


playerIdToInt : PlayerId -> Int
playerIdToInt pid =
    case pid of
        Unassigned -> Debug.crash "bad playerId"
        Zero -> 0
        One -> 1

getPlayer : PlayerId -> List Player -> Maybe Player
getPlayer pid players =
    case pid of
        Unassigned -> Nothing
        _ -> List.find
                (\p -> (playerIdToInt pid) == p.playerId)
                players

isYourTurn : PlayerId -> Model -> Bool
isYourTurn playerId model =
    Debug.crash "calculate if passed in id matches that of first player in the model"


initialModel : Model
initialModel = Model (Game [] (Board Dict.empty) (Bag []) [])
               Dict.empty Dict.empty Nothing Nothing Set.empty False