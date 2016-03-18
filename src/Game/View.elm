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
import Color exposing (darkBrown, black, red, lightBrown, lightGrey, blue)
import Dict
import Text
import Signal exposing (Address)
import Maybe.Extra as Maybe


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
        , Html.fromElement (viewBoardAndRack context model)
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
viewBoardAndRack : Context -> Model -> Element
viewBoardAndRack ({boardWidth, boardHeight} as context) model =
    -- TODO top level doesn't necessarily need to be a collage
    Graphics.collage (boardWidth+100) (boardHeight+100+100)
        [ viewBackground context model
        , viewTiles context model
        ]


viewBackground : Context -> Model -> Form
viewBackground context model =
    let boardBackground =
            Graphics.group [ boardBase context
                           , viewSquares context model
                           ]

        rackBackground =
            Graphics.group [ rackBase context ]

    in  Graphics.group
            [ boardBackground
                |> Graphics.moveY 50
            , rackBackground
                |> Graphics.moveY ( negate (toFloat context.boardHeight) / 2 - 50 )
            ]


boardBase : Context -> Form
boardBase {boardWidth, boardHeight} =
    rect (toFloat boardWidth+100) (toFloat boardHeight+100)
        |> filled darkBrown


rackBase : Context -> Form
rackBase {boardWidth} =
    rect (toFloat boardWidth + 100) (100)
        |> filled blue


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
viewTiles ({boardWidth, boardHeight, playerId} as context)
           {game, dragOffsets, rackDragOffsets} =
    let squareWidth = (toFloat boardWidth) / 14

        squareHeight = (toFloat boardHeight) / 14

        playerIdToInt pid =
            case pid of
                Unassigned -> Debug.crash "bad playerId"
                Zero -> 0
                One -> 1

        getPlayer pid players =
            case pid of
                Unassigned -> Nothing
                _ -> List.find
                        (\p -> (playerIdToInt pid) == p.playerId)
                        players

    in Graphics.toForm
        <| Graphics.collage (boardWidth + 100) (boardHeight + 100 + 100)
            (( Dict.toList game.gameBoard.contents
                |> List.filterMap
                    ( \(point,square) ->
                        let dragOffset =
                                Maybe.withDefault (0,0)
                                    (Dict.get point dragOffsets)

                            boardOffset = boardToXY context point
                            globalOffset = 50
                        in square.tile
                            |> Maybe.map
                                ( Graphics.move dragOffset
                                    << Graphics.move boardOffset
                                    << Graphics.moveY globalOffset
                                    << viewTile context (BoardIndex point) squareWidth squareHeight
                                )
                    )
            ) ++
            ( Maybe.mapDefault [] (.playerRack >> .rackTiles) (getPlayer playerId game.gamePlayers)
                |> List.indexedMap
                    ( \i tile ->
                        let dragOffset =
                                Maybe.withDefault (0,0)
                                    (Dict.get i rackDragOffsets)

                            rackOffset = (-50 + 30 * i,0)

                            globalOffset = negate (toFloat context.boardHeight) / 2 - 50

                        in  viewTile context (RackIndex i) squareWidth squareHeight tile
                                |> Graphics.move dragOffset
                                |> Graphics.moveY globalOffset
                                |> Graphics.move rackOffset

                    )

            ))



-- Project the point from boardspace to R2
boardToXY : Context -> Point -> Offset
boardToXY {boardWidth, boardHeight} (x,y) =
    let both f (x, y) = (f x, f y)
    in (x,y)
        |> both (\a -> a - 7)
        |> both toFloat
        |> (\(x',y') -> (x' * (toFloat boardWidth) / 14, negate <| y' * (toFloat boardHeight) / 14))


-- TODO recompute square width and sqaure heigh on the fly from context
viewTile : Context -> TileIndex -> Float -> Float -> Tile -> Form
viewTile {hoverAddress} index squareWidth squareHeight t =
    Graphics.centered (Text.fromString (toString t.tileLetter))
        |> Graphics.container (round squareWidth) (round squareHeight) Graphics.middle
        |> Graphics.hoverable
            ( Signal.message hoverAddress
                << \h -> if h then Just index else Nothing
            )
        |> Graphics.color lightGrey
        |> Graphics.toForm
        |> Graphics.scale 0.8
