module Game.View where


import Game.Model as Game exposing (Model, Player, PlayerId(..),Point)
import Game.Update as Game exposing (Action)
import Html exposing (Html, div, text)
import Signal exposing (Address)
import List.Extra as List
-- import Dict
-- import Html.Attributes
import Graphics.Element as Graphics exposing (Element)
import Graphics.Collage as Graphics exposing (Form)
import Color exposing (darkBrown)


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


boardBackground : Context -> Form
boardBackground {boardWidth, boardHeight} =
    Graphics.rect (toFloat boardWidth) (toFloat boardHeight)
        |> Graphics.filled darkBrown


square : Context -> Model -> Form
square = Debug.crash "foooo"

squares : Context -> Model -> Form
squares context model =
    let layout =
            List.groupBy (\(a,_) (c,_) -> a == c) <|
                [0..14] `List.andThen` \x ->
                [0..14] `List.andThen` \y ->
                [(x,y)]
    in Debug.crash "yoo"



-- Display the board
viewBoard : Context -> Model -> Element
viewBoard ({boardWidth, boardHeight} as context) model =
    Graphics.collage boardWidth boardHeight
        [ boardBackground context
        , squares context model
        ]




    --     viewBoardRow : List Point -> Html
    --     viewBoardRow row =
    --         Html.tr [] <|
    --             List.map viewTile row
    --
    --     viewTile : Point -> Html
    --     viewTile pt =
    --         Html.td
    --             [ Html.Attributes.style
    --                 [ ("border", "1px solid black") ]
    --             ]
    --             [ text
    --                 ( Dict.get pt gameBoard
    --                     |> Maybe.map (toString << .tileLetter)
    --                     |> Maybe.withDefault "Blank"
    --                 )
    --             ]
    --
    -- in div []
    --     [ Html.table
    --         [ Html.Attributes.style
    --             [ ("border", "1px solid black")
    --             , ("border-collapse", "collapse")
    --             ]
    --         ]
    --         [ Html.tbody []
    --             ( List.groupBy (\(a,_) (c,_) -> a == c) points
    --                 |> List.map viewBoardRow
    --             )
    --         ]
    --
    --     ]



enumFromTo : Int -> Int -> List Int
enumFromTo from to =
    if from >= to
        then []
        else from :: enumFromTo (from + 1) to


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