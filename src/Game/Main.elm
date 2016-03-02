module Main where


import Game.Model as Game exposing (Model)
import Game.Update as Game exposing (Action)
import Game.View as Game
import Game.Decode as GD
import StartApp exposing (App)
import Effects exposing (Never)
import Html exposing (Html)
import Task exposing (Task)
import Dict


{- A testbed main module for displaying
   the game

-}


app : App Model
app =
    -- TODO Remove this when decoding story gets fixed in elm and haskell
    let ungodlyHackedGame =
        GD.decodeGame gameString
            |> Result.map ( \g -> { g
                                  | gameBoard = Game.Board <|
                                      Dict.update (7,7)
                                        (Maybe.map (\s -> { s | tile = Just <| Game.Tile Game.P 4 } ))
                                      <| Dict.update (1,2)
                                            (Maybe.map (\s -> { s | tile = Just <| Game.Tile Game.A 2 } ))
                                            g.gameBoard.contents
                                  }
                          )

    in StartApp.start
        { init = ( Result.withDefault Game.initialModel ungodlyHackedGame, Effects.none)
        , update = Game.update
        , view = Game.view (Game.Context Game.One 500 500)
        , inputs = []
        }


gameString : String
gameString =
    "{\"gameBoard\":[],\"gameBag\":\"TRDGINRDBSENSRIDTIUPOAVNAERGNFOIASAHVLAIAELZUOQEECIOEJEOU_EHLWETABMEAA_FMPIONIWIGTYENC\",\"gameTurns\":[],\"gamePlayers\":[{\"playerName\":\"foo\",\"playerRack\":\"DOEUKLX\",\"playerType\":\"Human\",\"playerScore\":0,\"playerId\":0},{\"playerName\":\"bar\",\"playerRack\":\"RRTSYOT\",\"playerType\":\"Human\",\"playerScore\":0,\"playerId\":1}]}"


port tasks : Signal (Task Never ())
port tasks = app.tasks


main : Signal Html
main = app.html