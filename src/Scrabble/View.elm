module Scrabble.View where

import Signal exposing (Address)
import Html exposing (Html)
import Html.Events as Events
import Scrabble.Model exposing (Model)
import Scrabble.Update exposing (Action(..))
import Game.View as Game
import Html.Attributes as Attributes
import D3


view : Address Action -> Model -> Html
view address model =
    Html.div []
        [ Html.div []
            [ Html.text "Game State: "
            , Game.view (Signal.forwardTo address GameAction) model.game
            ]
        , Html.div []
            [ Html.input
                [ Attributes.placeholder "Enter command here"
                , Attributes.value model.command
                , Events.on "input" Events.targetValue
                    (\command -> Signal.message address (EditCommand command))
                ]
                []
            ]
        , Html.div []
            [ Html.button
                [ Events.onClick address SendMove ]
                [ Html.text "Send Command" ]
            ]
        , Html.div []
            [ Html.text ("D3 Version: " ++ D3.version)
            , Html.text ("I Am player " ++ toString model.playerId)
            ]

        ]
