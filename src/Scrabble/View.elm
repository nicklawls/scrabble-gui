module Scrabble.View where


import Signal exposing (Address)
import Html exposing (Html)
import Html.Events as Events
import Scrabble.Model exposing (Model, GameState(..))
import Scrabble.Update exposing (Action(..))
import Game.View as Game
import Game.Model exposing (Point)
import Html.Attributes as Attributes



type alias Context =
    { hoverAddress : Address (Maybe Point) }


view : Context -> Address Action -> Model -> Html
view context address model =
    -- TODO Add a shell that provides a header and centers everything
    case model.state of
        SignIn ->
            signIn address model

        Waiting ->
            waiting address model

        Playing ->
            gamePlay context address model


-- TODO disable input when it's not your turn
gamePlay : Context -> Address Action -> Model -> Html
gamePlay {hoverAddress} address model =
    Html.div []
        [ Html.div []
            [ Html.text "Game State: "
            , Game.view
                (Game.Context model.playerId 500 500 hoverAddress)
                (Signal.forwardTo address GameAction) model.game
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
            [ Html.text ("I Am player " ++ toString model.playerId)
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