module Game.View where


import Game.Model as Game
import Game.Update as Game exposing (Action)
import Html exposing (Html, div, text)
import Signal exposing (Address)

type alias Context =
    { playerId : Game.PlayerId }


view : Context -> Address Action -> Game.Model -> Html
view context address {gamePlayers, gameBoard, gameBag, gameTurns} =
    div []
        [ div [] [ text <| toString gamePlayers ]
        , div [] [ text <| toString gameBoard ]
        , div [] [ text <| toString gameBag ]
        , div [] [ text <| toString gameTurns ]
        ]


