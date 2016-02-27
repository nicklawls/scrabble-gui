module Game.View where


import Game.Model as Game exposing (Tile, Model, Player, PlayerId(..),Point)
import Game.Update as Game exposing (Action)
import Html exposing (Html, div, text)
import Signal exposing (Address)
import List.Extra as List
import Dict
import Graphics.Element exposing (Element, flow, right, down, centered, size, color, layers, sizeOf)
--import Graphics.Collage exposing (Form, collage, outlined, rect, toForm, defaultLine)
import Text
import Color exposing (lightBrown,black)
import Game.Encode exposing (letterString)


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

        viewBoardRow : List Point -> Element
        viewBoardRow row =
            flow right <|
                List.map
                    (  flip Dict.get boardDict
                    >> Maybe.withDefault (Game.Tile Game.Blank 0)
                    >> tile
                    ) row


    in Html.fromElement <| flow down
        ( List.groupBy (\(a,_) (c,_) -> a == c) points
            |> List.map viewBoardRow
        )


enumFromTo : Int -> Int -> List Int
enumFromTo from to =
    if from >= to
        then []
        else from :: enumFromTo (from + 1) to


tile : Tile -> Element
tile t =
    letterString t.tileLetter
        |> Text.fromString
        |> Text.height 40
        |> centered
        |> size 47 47
        |> color lightBrown
        |> putInBox'


putInBox' : Element -> Element
putInBox' e =
    Graphics.Element.container 50 50 Graphics.Element.middle e
        |> color black


-- display the local player's personal rack
-- TODO Store id as PlayerId within Player
viewRack : Context -> Model -> Html
viewRack {playerId} {gamePlayers} =
    let playerIdToInt pid =
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
            Html.fromElement
                ( flow right <| List.map tile playerRack.rackTiles )