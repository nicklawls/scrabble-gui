module Game.View where


import Game.Model as Game exposing (Model, Player, PlayerId(..),Point)
import Game.Update as Game exposing (Action)
import Html exposing (Html, div, text)
import Signal exposing (Address)
import List.Extra as List
import Dict


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
                    List.map (div [] << List.singleton << text)
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
viewBoard {gameBoard} =
    let boardDict =
            Dict.fromList gameBoard.contents

        points =
            enumFromTo 0 15 `List.andThen` \x ->
            enumFromTo 0 15 `List.andThen` \y ->
            [(x,y)]

        viewBoardRow : List Point -> Html
        viewBoardRow row =
            Html.tr [] <|
                List.map viewTile row

        viewTile : Point -> Html
        viewTile pt =
            Html.td []
                [ text
                    ( Dict.get pt boardDict
                        |> Maybe.map (toString << .tileLetter)
                        |> Maybe.withDefault "Blank"
                    )
                ]

    in div []
        [ Html.table []
            [ Html.tbody []
                ( List.groupBy (\(a,_) (c,_) -> a == c) points
                    |> List.map viewBoardRow
                )
            ]

        ]



enumFromTo : Int -> Int -> List Int
enumFromTo from to =
    if from >= to
        then []
        else from :: enumFromTo (from + 1) to


-- display the local player's personal rack
-- TODO Talk to josh about alligning the IDs
-- TODO Store id as PlayerId within Player
viewRack : Context -> Model -> Html
viewRack {playerId} {gamePlayers} =
    let viewTile =
            div [] << List.singleton << text << toString

        playerIdToInt pid =
            case pid of
                Unassigned -> Debug.crash "bad playerId" 0
                One -> 0
                Two -> 1

        getPlayer pid players =
            case pid of
                Unassigned -> Nothing
                _ -> List.find
                        (\p -> (playerIdToInt pid) == p.playerId )
                        players

    in case getPlayer playerId gamePlayers of
        Nothing ->
            div [] [text "egregious error has befallen you"]

        Just {playerRack} ->
            div [] <|
                [text "Rack: "] ++
                    List.map (viewTile << .tileLetter) playerRack.rackTiles