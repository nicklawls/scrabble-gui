module Game.Encode where


import Game.Model exposing (..)
import Letter exposing (Letter(..))
import Json.Encode exposing (Value)
import Maybe.Extra
import Dict
import String


letterString : Letter -> String
letterString l =
    if l == Blank
        then "_"
        else toString l

-- (<<) = (.)
letter : Letter -> Value
letter = Json.Encode.string << letterString


tile : Tile -> Value
tile =
    letter << .tileLetter


tiles : List Tile -> Value
tiles tiles =
    tiles
        |> List.map (letterString << .tileLetter)
        |> String.concat
        |> Json.Encode.string


rack : Rack -> Value
rack = tiles << .rackTiles


bag : Bag -> Value
bag = tiles << .bagTiles


bonus : Bonus -> Value
bonus =
    Json.Encode.string << toString


point : Point -> Value
point (x,y) =
    [x,y]
        |> List.map Json.Encode.int
        |> Json.Encode.list


maybe : (a -> Value) -> Maybe a -> Value
maybe f m =
    Maybe.withDefault Json.Encode.null (Maybe.map f m)


square : Square -> Value
square s =
    Json.Encode.object
        [ ("squarePos", point s.squarePos)
        , ("tile", maybe tile s.tile)
        , ("bonus", bonus s.bonus)
        ]


board : Board -> Value
board {contents} =
    Dict.toList contents
        |> List.filter (\(_,sq) -> Maybe.Extra.isJust sq.tile)
        |> List.map
            ( \(pt,sq) ->
                Json.Encode.list
                    [ point pt
                    , tile <| Maybe.withDefault (Tile Blank 0) (sq.tile)
                    ]
            )
        |> Json.Encode.list


playerType : PlayerType -> Value
playerType = Json.Encode.string << toString


player : Player -> Value
player t =
    Json.Encode.object
        [ ("playerType", playerType t.playerType)
        , ("playerName", Json.Encode.string t.playerName)
        , ("playerRack", rack t.playerRack)
        , ("playerScore", Json.Encode.int t.playerScore)
        , ("playerId", Json.Encode.int t.playerId)
        ]


-- (>>) = flip (<<)
players : List Player -> Value
players =
    List.map player >> Json.Encode.list


tilePut : TilePut -> Value
tilePut tp =
    case tp of
        LetterTilePut t p ->
            Json.Encode.list
                [ point p
                , tile t
                , Json.Encode.int t.score
                ]
        BlankTilePut l p ->
            Json.Encode.list
                [ point p
                , letter l
                , Json.Encode.int 0
                ]


wordPut : WordPut -> Value
wordPut {wordPutTiles} =
    Json.Encode.object
        [ ("wordPutTiles", Json.Encode.list <| List.map tilePut wordPutTiles)]


turn : Turn -> Value
turn t =
    Json.Encode.object
        [ ("playerId", Json.Encode.int t.playerId)
        , ("tilesPlayed", wordPut t.tilesPlayed)
        , ("points", Json.Encode.int t.points)
        , ("rackRemainder", rack t.rackRemainder)
        , ("tilesTakenFromBag", tiles t.tilesTakenFromBag)
        ]


turns : List Turn -> Value
turns =
    List.map turn >> Json.Encode.list


game : Game -> Value
game g =
    Json.Encode.object
        [ ("gamePlayers", players g.gamePlayers)
        , ("gameBoard", board g.gameBoard)
        , ("gameBag", bag g.gameBag)
        , ("gameTurns", turns g.gameTurns)]


encodeGame : Game -> String
encodeGame g =
    Json.Encode.encode 0 <| game g


encodeMessage : ClientMessage -> String
encodeMessage (Message mType g wp) =
    Json.Encode.list
        [ Json.Encode.string (toString mType)
        , game g
        , wordPut wp
        ] |> Json.Encode.encode 0

