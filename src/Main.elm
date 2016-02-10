module Main where

import Scrabble

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
        , inputs =
            [ Signal.map (Scrabble.RecieveGame << Json.Decode.decodeString Game.game) recieveGame]
        }


foo : Int
foo = 42


sendMoveMailbox : Mailbox String
sendMoveMailbox = Signal.mailbox ""

port sendMove : Signal String
port sendMove = sendMoveMailbox.signal

port recieveGame : Signal String



port tasks : Signal (Task Never ())
port tasks = app.tasks

main : Signal Html
main = app.html
