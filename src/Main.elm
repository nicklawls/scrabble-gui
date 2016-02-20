module Main where


import Scrabble.Model as Scrabble
import Scrabble.View as Scrabble
import Scrabble.Update as Scrabble exposing (Action(..))
import Game.Model exposing (PlayerId(..))
import Game.Update as Game exposing (Action(..))
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
        , update = Scrabble.update (Scrabble.Context moveMailbox.address nameMailbox.address)
        , view = Scrabble.view
        , inputs = [ gameEvents ]
        }


moveMailbox : Mailbox String
moveMailbox = Signal.mailbox ""


nameMailbox : Mailbox String
nameMailbox = Signal.mailbox ""


port sendMove : Signal String
port sendMove = moveMailbox.signal


port sendName : Signal String
port sendName = nameMailbox.signal


port socketMessages : Signal String


gameEvents : Signal Scrabble.Action
gameEvents =
    let mkAction : String -> Scrabble.Action
        mkAction s =
            case s of
                "" -> NoOp

                "1" -> SetId Zero

                "2" -> SetId One

                str -> GameAction (RecieveGame (GD.decodeGame str))
    in Signal.map mkAction socketMessages


port tasks : Signal (Task Never ())
port tasks = app.tasks


main : Signal Html
main = app.html