module Game.Update where


import Game.Model as Game exposing (Game, Point)
import Effects exposing (Effects)
import Drag exposing (Action(..))
import Dict

type Action
    = RecieveGame (Result String Game)
    | TrackTile (Maybe (Point, Drag.Action))


update : Action -> Game.Model -> (Game.Model, Effects Action)
update action model =
    case action of
        RecieveGame (Ok game) ->
            noEffects { model | game = game }

        -- TODO muuuuch more robust, user-facing error handling
        RecieveGame (Err msg) ->
            noEffects (Debug.log ("error: " ++ msg) model)

        TrackTile (Just (point, Lift)) ->
            noEffects { model | dragOffsets = Dict.insert point (0,0) model.dragOffsets }

        TrackTile (Just (point, MoveBy offset)) ->
            noEffects { model | dragOffsets =
                            Dict.update point ( Maybe.map (moveBy offset)) model.dragOffsets
                      }

        TrackTile (Just (point, Release)) ->
            noEffects { model | dragOffsets = Dict.remove point model.dragOffsets}


        TrackTile Nothing ->
            noEffects model


noEffects : a -> (a, Effects b)
noEffects a = (a, Effects.none)


moveBy : (Int, Int) -> (Float, Float) -> (Float, Float)
moveBy ( dx, dy ) ( x, y ) =
    ( x + toFloat dx, y - toFloat dy )