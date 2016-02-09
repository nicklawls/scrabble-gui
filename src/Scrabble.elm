module Scrabble where

import Effects exposing (Effects)
import Task exposing (Task)
import Signal exposing (Address)
import Html exposing (Html)
import Html.Events as Events
import Html.Attributes as Attributes
import Scrabble.Game as Game exposing (Game)


-- will eventually be some record cooresponding
-- to the json from the server

type alias Model =
    { game : Game
    , command : String
    }


type Action
    = NoOp
    | EditCommand String
    | SubmitMove
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


update : Action -> Model -> (Model, Effects Action)
update action model =
    case action of
        NoOp ->
            (model, Effects.none)

        EditCommand command ->
            ( { model | command = command } , Effects.none )

        SubmitMove ->
            ( model, submitMove model )

        RecieveGame (Ok game) ->
            ( { model | game = game }, Effects.none )

        RecieveGame (Err error) -> -- do nothing for now
            ( model, Effects.none)


-- will eventually fire off http POST, for now just sends the init state
-- right back
submitMove : Model -> Effects Action
submitMove _ =
    Game.init
        |> Task.succeed
        |> Task.toResult
        |> Task.map RecieveGame
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
                [ Events.onClick address SubmitMove ]
                [ Html.text "Send Command" ]
            ]
        ]
