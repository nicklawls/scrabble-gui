module Scrabble.Model where

import Game.Model as Game


type alias Model =
    { game : Game.Model
    , command : String
    , state : GameState
    , playerId : Game.PlayerId
    , playerName : String
    }


{- could use GameState to model
   who's turn it is, but for now
   I'll stick with some boolean checks
   in the view to limit input
-}


type GameState
    = SignIn
    | Waiting
    | Playing


initialModel : Model
initialModel =
    Model Game.initialModel "" SignIn Game.Unassigned ""