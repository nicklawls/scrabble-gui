module Game.Update where


import Game.Model as Game exposing (Game, Point, Offset)
import Effects exposing (Effects)
import Drag exposing (Action(..))
import Dict

type Action
    = RecieveGame (Result String Game)
    | TrackTile (Maybe (Point, Drag.Action))


type alias Context =
    { boardWidth : Int
    , boardHeight : Int
    }


update : Context -> Action -> Game.Model -> (Game.Model, Effects Action)
update context action ({game} as model) =
    case action of
        RecieveGame (Ok game') ->
            noEffects { model | game = game' }

        -- TODO muuuuch more robust, user-facing error handling
        RecieveGame (Err msg) ->
            noEffects (Debug.log ("error: " ++ msg) model)

        TrackTile (Just (point, Lift)) ->
            noEffects { model
                      | dragOffsets =
                          Dict.insert point (0,0) model.dragOffsets
                      , dropoff = Just point
                      }

        -- TODO if you don't have to track offsets for more than one tile at a time
        -- change dragOffsets from Dict Point Offset to Maybe Offset for potential
        -- performance boost

        -- TODO Think about if you need to actually store the offsets at all, can you
        -- just store the value given to you by MoveBy?

        TrackTile (Just ((x,y) as point, MoveBy (dx,dy))) ->

            let updatedOffsets =
                    Dict.update point (Maybe.map (moveBy (dx,dy))) model.dragOffsets

            in noEffects  { model
                          | dragOffsets = updatedOffsets
                          , dropoff = -- initial location + boardspace (pixels moved by)
                              Dict.get point updatedOffsets
                                `Maybe.andThen`
                                    ( Just
                                        << (\(bx,by) -> (bx + x - 7, by + y - 7 ))
                                        << xyToBoard context
                                    )
                          }

        TrackTile (Just (point, Release)) ->
            noEffects { model
                      | dragOffsets =
                          Dict.remove point model.dragOffsets
                      , dropoff = Nothing
                      , game =
                          { game
                          | gameBoard = Game.Board
                            ( game.gameBoard.contents
                                |> Dict.update point (Maybe.map (\s -> {s | tile = Nothing }))

                                -- dig through a couple maybe layers to get a function that puts the
                                -- moved tile in its place. If any of the maybes is Nothing, apply
                                -- the identity funciton instead

                                -- chaining `andThen`s lets you operate on pipelines of heterogeneous data
                                -- (m a -> m b -> m c -> m d), while letting you name the results of
                                -- the previous computations, keep those names in scope (via right associativity)
                                -- and use the particular monad to bake effects (failure, state) in at the same time

                                |> Maybe.withDefault identity
                                    ( model.dropoff
                                        `Maybe.andThen` \drpff -> Dict.get point game.gameBoard.contents
                                        `Maybe.andThen` \movedSquare ->
                                          Just << Dict.update drpff <|
                                                    Maybe.map (\s -> { s | tile = movedSquare.tile })
                                    )
                            )
                          }
                      }

        TrackTile Nothing ->
            noEffects model


xyToBoard : Context -> Offset -> Point
xyToBoard {boardWidth, boardHeight} (dx,dy) =
    let both f (x,y)  = (f x, f y)
    in (dx, negate dy)
        |> (\(dx',dy') -> ((dx' * 14) / (toFloat boardWidth), (dy' * 14) / (toFloat boardHeight)))
        |> both round
        |> both (\a -> a + 7)


calcDropoff : Context -> Point -> Offset -> (Int, Int)
calcDropoff ({boardWidth, boardHeight} as context) (x,y) offset =
    case xyToBoard context offset of
        (dx,dy) -> (x + dx, y + dy)

noEffects : a -> (a, Effects b)
noEffects a = (a, Effects.none)


moveBy : (Int, Int) -> (Float, Float) -> (Float, Float)
moveBy ( dx, dy ) ( x, y ) =
    ( x + toFloat dx, y - toFloat dy )