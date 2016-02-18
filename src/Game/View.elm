module Game.View where


import Game.Model as Game exposing (Model, Player)
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
viewScoreboard {gamePlayers} =
    let viewPlayer : Player -> Html
        viewPlayer {playerName, playerId, playerScore} =
            div []
                [ div [] <|
                    List.map (div [] << List.repeat 1 << text)
                        [ "Player " ++ toString (playerId + 1) ++ ": " ++ playerName
                        , "Score: " ++ toString playerScore
                        ]
                ]
    in div [] ( gamePlayers
                    |> List.sortBy .playerId -- assures consistency across turns
                    |> List.map viewPlayer
              )


-- Display the board
viewBoard : Model -> Html
viewBoard model =
    div [] [text "board"]


-- display the local player's personal rack
viewRack : Context -> Model -> Html
viewRack context model =
    div [] [text "rack"]