module Game.View where


import Game.Model as Game
import Game.Update as Game exposing (Action)
import Html exposing (Html)
import Signal exposing (Address)

view : Address Action -> Game.Model -> Html
view _ model =
    Html.text <| toString model