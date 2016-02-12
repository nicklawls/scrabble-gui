module Main where

import Scrabble.Model as Scrabble
import Scrabble.View as Scrabble
import Scrabble.Update as Scrabble
import Game.Update as Game
import Game.Decode as GD
import StartApp exposing (App)
import Html exposing (Html)
import Task exposing (Task)
import Effects exposing (Never)
import Signal exposing (Mailbox)


app : App Scrabble.Model
app =
    StartApp.start
        { init = Scrabble.init
        , update = Scrabble.update (Scrabble.Context sendMoveMailbox.address)
        , view = Scrabble.view
        , inputs = [ gameEvents ]
        }


sendMoveMailbox : Mailbox String
sendMoveMailbox = Signal.mailbox ""


port sendMove : Signal String
port sendMove = sendMoveMailbox.signal


port socketMessages : Signal String


gameEvents : Signal Scrabble.Action
gameEvents =
    let mkAction : String -> Scrabble.Action
        mkAction s =
            case s of
                "" ->
                    Scrabble.NoOp

                "1" ->
                    Scrabble.SetId Scrabble.One

                "2" ->
                    Scrabble.SetId Scrabble.Two

                str ->
                    Scrabble.GameAction (Game.RecieveGame (GD.decodeGame str))
    in Signal.map mkAction socketMessages


port tasks : Signal (Task Never ())
port tasks = app.tasks


main : Signal Html
main = app.html
