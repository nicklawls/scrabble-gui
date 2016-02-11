module Scrabble where

import Effects exposing (Effects)
import Task exposing (Task)
import Signal exposing (Address)
import Html exposing (Html)
import Html.Events as Events
import Html.Attributes as Attributes
import Scrabble.Game as Game exposing (Game)
import D3


type alias Model =
    { game : Game
    , command : String
    }

type alias Context =
    { sendMoveAddress : Address String }


type Action
    = NoOp
    | EditCommand String
    | SendMove
    | RecieveGame (Result String Game)


-- Result is elm's Either
-- Result e a = Err e | Ok a


init : (Model, Effects Action)
init =
    ( { game = Game.init
      , command = ""
      }
    , Effects.task (Task.succeed NoOp)
    )


update : Context -> Action -> Model -> (Model, Effects Action)
update context action model =
    case action of
        NoOp ->
            (model, Effects.none)

        EditCommand command ->
            ( { model | command = command } , Effects.none )

        SendMove ->
            ( model, sendMove context model )

        RecieveGame (Ok game) ->
            ( { model | game = game }, Effects.none )

        RecieveGame (Err error) -> -- do nothing for now
            let log = Debug.log "game receipt error"
            in ( model, Effects.none)


-- will eventually fire off http POST, for now just sends the init state
-- right back
sendMove : Context -> Model -> Effects Action
sendMove {sendMoveAddress} model =
    model.command
        |> Signal.send sendMoveAddress
        |> Task.map (\_ -> NoOp)
        |> Effects.task


{- The above is the Elm-y way of doing it, making heavy use of flipped
   function application; (|>) : a -> (a -> b) -> b.
   You can achieve a more haskelly style with (<|), the equivalent of ($)
-}


view : Address Action -> Model -> Html
view address model =
    Html.div []
        [ Html.div []
            [ Html.text "Game State: "
            , Html.text (toString model.game)
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
            [Html.text ("D3 Version: " ++ D3.version) ]
        ]
