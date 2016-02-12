module Game.Encode where

import Game.Model exposing (..)
import Json.Encode exposing (Value)
import String


-- (<<) = (.)
letter : Letter -> Value
letter =
    Json.Encode.string << toString


tile : Tile -> Value
tile =
    letter << .tileLetter


tiles : List Tile -> Value
tiles tiles =
    tiles
        |> List.map (toString << .tileLetter)
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
board b =
    let entry (p,t) =
            Json.Encode.list [point p, tile t]
    in Json.Encode.object
        [ ("contents", Json.Encode.list
                        (List.map entry b.contents)
          )
        ]


playerType : PlayerType -> Value
playerType = Json.Encode.string << toString


player : Player -> Value
player t =
    Json.Encode.object
        [ ("playerType", playerType t.playerType)
        , ("playerName", Json.Encode.string t.playerName)
        , ("playerRack", rack t.playerRack)
        , ("playerScore", Json.Encode.int t.playerScore)
        , ("playerScore", Json.Encode.int t.playerId)
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
wordPut =
    .wordPutTiles >> List.map tilePut >> Json.Encode.list


turn : Turn -> Value
turn t =
    Json.Encode.object
        [ ("playerId", Json.Encode.int t.playerId)
        , ("wordPutTiles", wordPut t.wordPutTiles)
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
encodeGame =
    game >> Json.Encode.encode 0