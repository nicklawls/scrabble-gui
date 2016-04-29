module Main where


import BlankTilePicker.Model exposing (init, Model, PickerState(..))
import BlankTilePicker.View exposing (view, Context)
import BlankTilePicker.Update exposing (update)
import StartApp exposing (App)
import Signal exposing (Signal)
import Task exposing (Task)
import Effects exposing (Never)
import Html exposing (Html)


app : App Model
app =
    StartApp.start
        { init = (init, Effects.none)
        , update = update
        , view = view (Context (2,2))
        , inputs = []
        }


port tasks : Signal (Task Never ())
port tasks = app.tasks


main : Signal Html
main = app.html