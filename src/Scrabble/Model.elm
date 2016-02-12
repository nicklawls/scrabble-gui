module Scrabble.Model where

import Game.Model as Game


type alias Model =
    { game : Game.Model
    , command : String
    , playerId : PlayerId
    }


type PlayerId
    = Unassigned
    | One
    | Two


initialModel : Model
initialModel =
    Model Game.initialModel "" Unassigned
