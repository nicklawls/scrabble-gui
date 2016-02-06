module Main where

import Scrabble

import StartApp exposing (App)
import Html exposing (Html)
import Task exposing (Task)
import Effects exposing (Never)


app : App Scrabble.Model
app =
    StartApp.start
        { init = Scrabble.init
        , update = Scrabble.update
        , view = Scrabble.view
        , inputs = []
        }


port tasks : Signal (Task Never ())
port tasks = app.tasks


main : Signal Html
main = app.html
