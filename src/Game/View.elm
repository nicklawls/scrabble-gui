module Game.View where


import Game.Model as Game exposing (Model, Player, PlayerId(..),Point, Square)
import Game.Update as Game exposing (Action)
import Html exposing (Html, div, text)
import Signal exposing (Address)
import List.Extra as List
-- import Dict
-- import Html.Attributes
import Graphics.Element as Graphics exposing (Element, flow, down, right,empty, color, size)
import Graphics.Collage as Graphics exposing (Form, filled,rect)
import Color exposing (darkBrown, black, red, lightBrown)
import Dict


type alias Context =
    { playerId : Game.PlayerId
    , boardWidth : Int
    , boardHeight : Int
    }


-- display the game
view : Context -> Address Action -> Model -> Html
view context address model =
    div []
        [ viewScoreboard model
        , Html.fromElement (viewBoard context model)
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
viewBoard : Context -> Model -> Element
viewBoard ({boardWidth, boardHeight} as context) model =
    Graphics.collage (boardWidth+100) (boardHeight+100)
        [ boardBackground context
        , squares context model
        ]


boardBackground : Context -> Form
boardBackground {boardWidth, boardHeight} =
    rect (toFloat boardWidth+100) (toFloat boardHeight+100)
        |> filled darkBrown


squares : Context -> Model -> Form
squares context model =
        -- ensure that the squares get unpacked in order and all are accounted for
    let layout =
            List.groupBy (\(a,_) (c,_) -> a == c) <|
                [0..14] `List.andThen` \x ->
                [0..14] `List.andThen` \y ->
                [(x,y)]

    in Graphics.toForm << flow down <|
        List.map (boardRow context model) layout


boardRow : Context -> Model -> List Point -> Element
boardRow c m pts =
    flow right <|
        List.map (square c m) pts


square : Context -> Model -> Point -> Element
square {boardWidth, boardHeight} {gameBoard} pt =
    let squareWidth = (toFloat boardWidth) / 14

        squareHeight = (toFloat boardHeight) / 14

    in Graphics.collage (round squareWidth) (round squareHeight)
        << List.singleton
        <| case Dict.get pt gameBoard.contents of
                Just sqr ->
                    rect squareWidth squareHeight
                        |> filled lightBrown
                        --|> outlined (solid black)

                Nothing ->
                    Debug.log ("Square at point " ++ toString pt ++ " not present")
                              ( rect squareWidth squareHeight
                                    |> filled red
                              )


-- display the local player's personal rack
-- TODO Store id as PlayerId within Player
viewRack : Context -> Model -> Html
viewRack {playerId} {gamePlayers} =
    let viewTile =
            div [] << List.singleton << text << toString

        playerIdToInt pid =
            case pid of
                Unassigned -> Debug.crash "bad playerId" 0
                Zero -> 0
                One -> 1

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