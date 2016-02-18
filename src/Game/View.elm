module Game.View where


import Game.Model as Game exposing (Model)
import Game.Update as Game exposing (Action)
import Html exposing (Html, div, text)
import Signal exposing (Address)


type alias Context =
    { playerId : Game.PlayerId }


-- display the game
view : Context -> Address Action -> Model -> Html
view context address model =
    div []
        [ viewScoreboard model
        , viewBoard model
        , viewRack context model
        ]


-- Display the two players and their scores
viewScoreboard : Model -> Html
viewScoreboard model =
    Debug.crash "implement scoreboard"


-- Display the board
viewBoard : Model -> Html
viewBoard model =
    Debug.crash "implement board"


-- display the local player's personal rack
viewRack : Context -> Model -> Html
viewRack context model =
    Debug.crash "implement rack"