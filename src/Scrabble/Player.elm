module Scrabble.Player where

import Json.Decode exposing (Decoder, (:=))
import Scrabble.Board exposing (Rack, Score)


type PlayerType = Human | AI


playerType : Decoder PlayerType
playerType =
    let parsePlayerType str =
            case str of
                "Human" -> Ok Human
                "AI" -> Ok AI
                _ -> Err "PlayerType parse failure"
    in Json.Decode.customDecoder
        Json.Decode.string parsePlayerType


type alias Name = String


type alias Player =
    { playerType : PlayerType
    , playerName : Name
    , playerRack : Rack
    , playerScore : Score
    , playerId : Int
    }

player : Decoder Player
player =
    Json.Decode.object5
    (\t n r s i -> { playerType = t
                   , playerName = n
                   , playerRack = r
                   , playerScore = s
                   , playerId = i
                   }
    )
    ("playerType" := playerType)
    ("playerName" := Json.Decode.string )
    ("playerRack" := Scrabble.Board.rack)
    ("playerScore" := Json.Decode.int )
    ("playerId" := Json.Decode.int)
