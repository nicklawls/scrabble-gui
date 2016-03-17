module Game.View where


import Game.Model as Game exposing
    (Model, Player, PlayerId(..),Point, Square, Tile, Offset, TileIndex(..))
import Game.Update as Game exposing (Action)
import Html exposing (Html, div, text)
import Signal exposing (Address)
import List.Extra as List
import Graphics.Input as Graphics
import Graphics.Element as Graphics exposing (Element, flow, down, right,empty, color, size)
import Graphics.Collage as Graphics exposing (Form, filled,rect)
import Color exposing (darkBrown, black, red, lightBrown, lightGrey)
import Dict
import EveryDict
import Text
import Signal exposing (Address)


type alias Context =
    { playerId : Game.PlayerId
    , boardWidth : Int
    , boardHeight : Int
    , hoverAddress : Address (Maybe TileIndex)
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
viewScoreboard {game} =
    let viewPlayer : Player -> Html
        viewPlayer {playerName, playerId, playerScore} =
            div []
                [ div [] <|
                    List.map (div [] << List.singleton << text)
                        [ "Player " ++ toString (playerId + 1) ++ ": " ++ playerName
                        , "Score: " ++ toString playerScore
                        ]
                ]
    in div [] ( game.gamePlayers
                    |> List.sortBy .playerId -- assures consistency across turns
                    |> List.map viewPlayer
              )


-- Display the board
viewBoard : Context -> Model -> Element
viewBoard ({boardWidth, boardHeight} as context) model =
    -- TODO top level doesn't necessarily need to be a collage
    Graphics.collage (boardWidth+100) (boardHeight+100)
        [ boardBackground context
        , viewSquares context model
        , viewTiles context model
        ]


boardBackground : Context -> Form
boardBackground {boardWidth, boardHeight} =
    rect (toFloat boardWidth+100) (toFloat boardHeight+100)
        |> filled darkBrown


viewSquares : Context -> Model -> Form
viewSquares context model =
        -- ensure that the squares get unpacked in order and all are accounted for
    let layout =
            List.groupBy (\(a,_) (c,_) -> a == c) <|
                [0..14] `List.andThen` \x ->
                [0..14] `List.andThen` \y ->
                [(x,y)]

        viewBoardColumn c m pts =
            flow down <|
                List.map (viewSquare c m) pts

    in Graphics.toForm << flow right <|
        List.map (viewBoardColumn context model) layout


viewSquare : Context -> Model -> Point -> Element
viewSquare ({boardWidth, boardHeight} as context) {game} pt =
    let squareWidth = (toFloat boardWidth) / 14

        squareHeight = (toFloat boardHeight) / 14

    in Graphics.collage (round squareWidth) (round squareHeight)
        << List.singleton
        -- TODO Rewrite this with Maybe.andThen
        <| case Dict.get pt game.gameBoard.contents of
                Just sqr ->
                    Graphics.group <|
                        [ rect squareWidth squareHeight
                            |> filled lightBrown
                            -- TODO layer dots on top as necessary
                        ]

                Nothing ->
                    Debug.log ("Square at point " ++ toString pt ++ " not present")
                              ( rect squareWidth squareHeight
                                    |> filled red
                              )

-- if the square has a tile, render it on top of the rect

-- ++ Maybe.mapDefault []
--     ( List.singleton
--         << Graphics.move offset
--         << viewTile context pt squareWidth squareHeight
--     ) sqr.tile

{- Get all the tiles on the board and apply the global positon offset
   For the tiles being tracked, apply the local (drag and drop) offset

-}
viewTiles : Context -> Model -> Form
viewTiles ({boardWidth, boardHeight} as context) {game, dragOffsets} =
    let squareWidth = (toFloat boardWidth) / 14

        squareHeight = (toFloat boardHeight) / 14

    in Graphics.toForm
        <| Graphics.collage (boardWidth + 100) (boardHeight + 100)
            ( Dict.toList game.gameBoard.contents
                |> List.filterMap
                    ( \(point,square) ->
                        let dragOffset =
                                Maybe.withDefault (0,0)
                                    (EveryDict.get (BoardIndex point) dragOffsets)

                            boardOffset = boardToXY context point
                        in square.tile
                            |> Maybe.map
                                ( Graphics.move dragOffset
                                    << Graphics.move boardOffset
                                    << viewTile context point squareWidth squareHeight
                                )
                    )
            )


-- Project the point from boardspace to R2
boardToXY : Context -> Point -> Offset
boardToXY {boardWidth, boardHeight} (x,y) =
    let both f (x, y) = (f x, f y)
    in (x,y)
        |> both (\a -> a - 7)
        |> both toFloat
        |> (\(x',y') -> (x' * (toFloat boardWidth) / 14, negate <| y' * (toFloat boardHeight) / 14))



viewTile : Context -> Point -> Float -> Float -> Tile -> Form
viewTile {hoverAddress} p squareWidth squareHeight t =
    Graphics.centered (Text.fromString (toString t.tileLetter))
        |> Graphics.container (round squareWidth) (round squareHeight) Graphics.middle
        |> Graphics.hoverable
            ( Signal.message hoverAddress
                << \h -> if h then Just (BoardIndex p) else Nothing
            )
        |> Graphics.color lightGrey
        |> Graphics.toForm
        |> Graphics.scale 0.8


-- display the local player's personal rack
-- TODO Store id as PlayerId within Player
viewRack : Context -> Model -> Html
viewRack {playerId} {game} =
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

    in case getPlayer playerId game.gamePlayers of
        Nothing ->
            div [] [text "egregious error has befallen you"]

        Just {playerRack} ->
            div [] <|
                [text "Rack: "] ++
                    List.map (viewTile << .tileLetter) playerRack.rackTiles