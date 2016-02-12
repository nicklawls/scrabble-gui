module Scrabble.Model where

import Scrabble.Game as Game exposing (Game)


type alias Model =
    { game : Game
    , command : String
    }


initialModel : Model
initialModel =
    Model Game.init ""
