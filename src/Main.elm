module Main where


import Scrabble.Model as Scrabble
import Scrabble.View as Scrabble
import Scrabble.View
import Scrabble.Update as Scrabble exposing (Action(..))
import Scrabble.Update
import Game.Model exposing (PlayerId(..), Point)
import Game.Update as Game exposing (Action(..))
import Game.Decode as GD
import StartApp exposing (App)
import Html exposing (Html)
import Task exposing (Task)
import Effects exposing (Never)
import Signal exposing (Mailbox)
import Drag


app : App Scrabble.Model
app =
    StartApp.start
        { init = Scrabble.init
        , update = Scrabble.update (Scrabble.Update.Context moveMailbox.address nameMailbox.address)
        , view = Scrabble.view (Scrabble.View.Context hover.address)
        , inputs =
            [ gameEvents
            , Signal.map
                (Scrabble.GameAction << Game.TrackTile)
                (Drag.trackMany Nothing hover.signal)
            ]
        }


moveMailbox : Mailbox String
moveMailbox = Signal.mailbox ""


nameMailbox : Mailbox String
nameMailbox = Signal.mailbox ""


hover : Signal.Mailbox (Maybe Point)
hover = Signal.mailbox Nothing


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