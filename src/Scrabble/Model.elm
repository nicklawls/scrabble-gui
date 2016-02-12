module Scrabble.Model where

import Game.Model as Game


type alias Model =
    { game : Game.Model
    , command : String
    }


initialModel : Model
initialModel =
    Model Game.initialModel ""
