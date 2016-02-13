module Game.Model where


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


{- TODO in Board, change `Tile` to `Square` and use the position to infer
   the bonus at that square. Will have to do something similar
   to `Tile`, keeping a lookup table of bonuses and retreieving
   them when the position gets parsed
-}


type alias Board =
    { contents : List (Point, Tile) }


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

type alias Model = Game


initialModel : Model
initialModel = Game [] (Board []) (Bag []) []