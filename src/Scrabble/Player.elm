module Scrabble.Player where

import Json.Decode exposing (Decoder, (:=))
import Scrabble.Board as Board exposing (Rack, Score)
import Json.Encode exposing (Value)

type PlayerType = Human | AI


encodePlayerType : PlayerType -> Value
encodePlayerType = Json.Encode.string << toString


playerType : Decoder PlayerType
playerType =
    let parsePlayerType str =
            case str of
                "Human" -> Ok Human
                "AI" -> Ok AI
                _ -> Err ("PlayerType parse failure: " ++ str)
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


encodePlayer : Player -> Value
encodePlayer t =
    Json.Encode.object
        [ ("playerType", encodePlayerType t.playerType)
        , ("playerName", Json.Encode.string t.playerName)
        , ("playerRack", Board.encodeRack t.playerRack)
        , ("playerScore", Json.Encode.int t.playerScore)
        , ("playerScore", Json.Encode.int t.playerId)
        ]


encodePlayers : List Player -> Value
encodePlayers ps =
    ps
        |> List.map encodePlayer 
        |> Json.Encode.list


player : Decoder Player
player =
    Json.Decode.object5 Player
        ("playerType" := playerType)
        ("playerName" := Json.Decode.string )
        ("playerRack" := Board.rack)
        ("playerScore" := Json.Decode.int )
        ("playerId" := Json.Decode.int)
