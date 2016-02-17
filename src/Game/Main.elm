module Main where


import Game.Model as Game exposing (Model)
import Game.Update as Game exposing (Action)
import Game.View as Game
import Game.Decode as GD
import StartApp exposing (App)
import Effects exposing (Never)
import Html exposing (Html)
import Task exposing (Task)

{- A testbed main module for displaying
   the game

-}


app : App Model
app = StartApp.start
        { init = (Result.withDefault Game.initialModel (GD.decodeGame gameString), Effects.none)
        , update = Game.update
        , view = Game.view
        , inputs = []
        }


gameString : String
gameString =
    "{\"gameBoard\":[],\"gameBag\":\"TRDGINRDBSENSRIDTIUPOAVNAERGNFOIASAHVLAIAELZUOQEECIOEJEOU_EHLWETABMEAA_FMPIONIWIGTYENC\",\"gameTurns\":[],\"gamePlayers\":[{\"playerName\":\"foo\",\"playerRack\":\"DOEUKLX\",\"playerType\":\"Human\",\"playerScore\":0,\"playerId\":0},{\"playerName\":\"bar\",\"playerRack\":\"RRTSYOT\",\"playerType\":\"Human\",\"playerScore\":0,\"playerId\":1}]}"


port tasks : Signal (Task Never ())
port tasks = app.tasks


main : Signal Html
main = app.html