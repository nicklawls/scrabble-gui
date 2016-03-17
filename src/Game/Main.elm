module Main where


import Game.Model as Game exposing (Model)
import Game.Update as Game exposing (Action)
import Game.View as Game
import Game.View
import Game.Update
import Game.Decode as GD
import StartApp exposing (App)
import Effects exposing (Never)
import Html exposing (Html)
import Task exposing (Task)
import Dict
import EveryDict
import Drag


{- A testbed main module for displaying
   the game

-}


app : App Model
app =
    -- TODO Remove this when decoding story gets fixed in elm and haskell
    let ungodlyHackedModel =
        GD.decodeGame gameString
            |> Result.map
                  ( \g -> { g
                          | gameBoard = Game.Board <|
                              Dict.update (7,7)
                                (Maybe.map (\s -> { s | tile = Just <| Game.Tile Game.P 4 } ))
                              <| Dict.update (8,7)
                                    (Maybe.map (\s -> { s | tile = Just <| Game.Tile Game.A 2 } ))
                                <| Dict.update (1,2)
                                      (Maybe.map (\s -> { s | tile = Just <| Game.Tile Game.T 2 } ))
                                      g.gameBoard.contents
                          }
                  )
            |> Result.map (\g -> Game.Model g EveryDict.empty Nothing)

    in StartApp.start
        { init = ( Result.withDefault Game.initialModel ungodlyHackedModel, Effects.none)
        , update = Game.update (Game.Update.Context 500 500)
        , view = Game.view (Game.View.Context Game.One 500 500 hover.address)
        , inputs = [Signal.map Game.TrackTile (Drag.trackMany Nothing hover.signal)]
        }


hover : Signal.Mailbox (Maybe Game.Point)
hover = Signal.mailbox Nothing



gameString : String
gameString =
    "{\"gameBoard\":[],\"gameBag\":\"TRDGINRDBSENSRIDTIUPOAVNAERGNFOIASAHVLAIAELZUOQEECIOEJEOU_EHLWETABMEAA_FMPIONIWIGTYENC\",\"gameTurns\":[],\"gamePlayers\":[{\"playerName\":\"foo\",\"playerRack\":\"DOEUKLX\",\"playerType\":\"Human\",\"playerScore\":0,\"playerId\":0},{\"playerName\":\"bar\",\"playerRack\":\"RRTSYOT\",\"playerType\":\"Human\",\"playerScore\":0,\"playerId\":1}]}"


port tasks : Signal (Task Never ())
port tasks = app.tasks


main : Signal Html
main = app.html