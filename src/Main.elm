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
        , update = Scrabble.update {sendMoveAddress = sendMoveMailbox.address}
        , view = Scrabble.view
        , inputs = [ gameState ]
        }


sendMoveMailbox : Mailbox String
sendMoveMailbox = Signal.mailbox ""


port sendMove : Signal String
port sendMove = sendMoveMailbox.signal


port recieveGame : Signal String


gameState : Signal Scrabble.Action
gameState =
    Signal.map
        (Scrabble.GameAction << Game.RecieveGame << GD.decodeGame) recieveGame


port tasks : Signal (Task Never ())
port tasks = app.tasks


main : Signal Html
main = app.html
