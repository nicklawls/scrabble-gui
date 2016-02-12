module Main where

import Scrabble.Model as Scrabble
import Scrabble.View as Scrabble
import Scrabble.Update as Scrabble 
import StartApp exposing (App)
import Html exposing (Html)
import Task exposing (Task)
import Effects exposing (Never)
import Signal exposing (Mailbox)
import Scrabble.Game as Game exposing (Game)
import Json.Decode

app : App Scrabble.Model
app =
    StartApp.start
        { init = Scrabble.init
        , update = Scrabble.update {sendMoveAddress = sendMoveMailbox.address}
        , view = Scrabble.view
        , inputs = -- TODO make parsing the responsibility of Scrabble, since app level errors may occur
            [ Signal.map (Scrabble.RecieveGame << Json.Decode.decodeString Game.game) recieveGame]
        }


sendMoveMailbox : Mailbox String
sendMoveMailbox = Signal.mailbox ""


port sendMove : Signal String
port sendMove = sendMoveMailbox.signal


port recieveGame : Signal String


port tasks : Signal (Task Never ())
port tasks = app.tasks

main : Signal Html
main = app.html
