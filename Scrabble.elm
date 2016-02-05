module Scrabble where

import Effects exposing (Effects, Never)
import Task exposing (Task)
import Signal exposing (Address)
import Html exposing (Html)
import Html.Events as Events
import Html.Attributes as Attributes
import StartApp exposing (App)
import String

-- will eventually be some record cooresponding
-- to the json from the server
type alias GameState = String


type alias Model =
    { gameState : GameState
    , command : String
    }


type Action
    = NoOp
    | EditCommand String
    | SendCommand
    | RecieveState (Result String GameState)

-- Result is elm's Either
-- Result e a = Err e | Ok a

init : (Model, Effects Action)
init =
    ( { gameState = "InitialGame"
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

        SendCommand ->
            ( model, sendCommand model )

        RecieveState (Ok state) ->
            ( { model | gameState = state }, Effects.none )

        RecieveState (Err error) -> -- do nothing for now
            ( model, Effects.none)


-- will eventually fire off http POST, for now just uppercases the command
-- and passes it straight back as the new state
sendCommand : { model | command : String } -> Effects Action
sendCommand model =
    model.command
        |> String.toUpper
        |> String.append "Capitalized Command: "
        |> Task.succeed
        |> Task.toResult
        |> Task.map RecieveState
        |> Effects.task


    {- The above is the Elm-y way of doing it, making heavy use of flipped
       function application; (|>) : a -> (a -> b) -> b.
       The following more haskelly approach is equivalent:

    Effects.task (Task.map RecieveState (Task.toResult (Task.succeed ("Capitalized Command: " ++ (String.toUpper model.command)))))
    -}


view : Address Action -> Model -> Html
view address model =
    Html.div []
        [ Html.div []
            [ Html.text "Game State: "
            , Html.text model.gameState
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
                [ Events.onClick address SendCommand ]
                [ Html.text "Send Command" ]
            ]
        ]


app : App Model
app =
    StartApp.start
        { init = init
        , update = update
        , view = view
        , inputs = []
        }


port tasks : Signal (Task Never ())
port tasks = app.tasks


main : Signal Html
main = app.html
