module Main where


import BlankTilePicker.Model exposing (init)
import BlankTilePicker.Update
import BlankTilePicker.View exposing (view)

import Html

main : Html.Html
main = view (Debug.crash "foo") init