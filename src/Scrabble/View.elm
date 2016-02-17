module Scrabble.View where

import Signal exposing (Address)
import Html exposing (Html)
import Html.Events as Events
import Scrabble.Model exposing (Model, GameState(..))
import Scrabble.Update exposing (Action(..))
import Game.View as Game
import Html.Attributes as Attributes
import D3


view : Address Action -> Model -> Html
view address model =
    -- TODO Add a shell that provides a header and centers everything
    case model.state of
        SignIn ->
            signIn address model

        Waiting ->
            waiting address model

        Playing ->
            gamePlay address model


-- TODO disable input when it's not your turn
gamePlay : Address Action -> Model -> Html
gamePlay address model =
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


signIn : Address Action -> Model -> Html
signIn address model =
    Html.div []
        [ Html.div []
            [ Html.input
                [ Attributes.placeholder "Enter name here"
                , Attributes.value model.playerName
                , Events.on "input" Events.targetValue
                    (\name -> Signal.message address (EditName name))
                ]
                []
            ]
        , Html.div []
            [ Html.button
                [ Events.onClick address SendName ]
                [ Html.text "Submit Name" ]
            ]
        ]


waiting : Address Action -> Model -> Html
waiting address model =
    Html.div []
        [ Html.text "Waiting for opponent" ]