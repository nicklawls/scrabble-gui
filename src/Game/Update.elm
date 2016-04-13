module Game.Update where


import Game.Model as Game exposing (Game, Point, Offset, TileIndex(..), getPlayer, isYourTurn)
import Effects exposing (Effects)
import Drag exposing (Action(..))
import Dict
import Set
import Maybe.Extra as Maybe
import List.Extra as List
import Signal exposing (Address)
import Task
import Game.Encode as Game

type Action
    = NoOp
    | SendMove
    | RecieveGame (Result String Game)
    | RecieveCheck (Result String Bool)
    | TrackTile (Maybe (TileIndex, Drag.Action))


type alias Context =
    { playerId : Game.PlayerId
    , boardWidth : Int
    , boardHeight : Int
    , moveAddress : Address String
    }


-- chaining `andThen`s lets you operate on pipelines of heterogeneous data
-- (m a -> m b -> m c -> m d), while letting you name the results of
-- the previous computations, keep those names in scope (via right associativity)
-- and use the particular monad to bake effects (failure, state) in at the same time


update : Context -> Action -> Game.Model -> (Game.Model, Effects Action)
update context action ({game} as model) =
    case action of
        NoOp ->
            noEffects model

        SendMove ->
            ( model, sendMessage context model Game.ActualMove)

        RecieveGame (Ok game') ->
            -- TODO be wary for other things that have to get reset on each new turn
            noEffects { model
                      | game = game'
                      , initialGameState = game'
                      , dropoff = Nothing
                      , rackDropoff = Nothing
                      , dragOffsets = Dict.empty
                      , rackDragOffsets = Dict.empty
                      , boardOrigins = Set.empty
                      }

        -- TODO muuuuch more robust, user-facing error handling
        RecieveGame (Err msg) ->
            noEffects (Debug.log ("error: " ++ msg) model)

        RecieveCheck (Ok check) ->
            noEffects {model | prevMoveValid = check}

        RecieveCheck (Err msg) ->
            noEffects (Debug.log ("error: " ++ msg) model)

        TrackTile (Just ((BoardIndex point), Lift)) ->
            noEffects { model
                      | dragOffsets =
                          Dict.insert point (0,0) model.dragOffsets
                      , dropoff = Just point
                      , rackDropoff = Nothing
                      }

        TrackTile (Just ((RackIndex i), Lift)) ->
            noEffects { model
                      | rackDragOffsets =
                          Dict.insert i (0,0) model.rackDragOffsets
                      , rackDropoff = Just i
                      }

        TrackTile (Just (BoardIndex ((x,y) as point), MoveBy (dx,dy))) ->

            let updatedOffsets =
                    Dict.update point (Maybe.map (moveBy (dx,dy))) model.dragOffsets

                updatedPoint =
                    Dict.get point updatedOffsets
                        `Maybe.andThen`
                          ( Just
                              << (\(bx,by) -> (bx + x - 7, by + y - 7 ))
                              << xyToBoard context
                          )

            in noEffects <|
                if Maybe.mapDefault False
                    (\(x',y') -> (x' >= 0 && x' < 15 && y' >= 0 && y' < 15)) updatedPoint
                then  { model
                      | dragOffsets = updatedOffsets
                      , dropoff = updatedPoint -- initial location + boardspace (pixels moved by)
                      , rackDropoff = Nothing
                      }

                else if -- within on x bounds but large on y bounds
                    Maybe.mapDefault False
                        (\(x',y') -> (x' >= 0 && x' < 15 && y' >= 0)) updatedPoint
                then  { model
                      | dragOffsets = updatedOffsets
                      , dropoff = Nothing
                      , rackDropoff = Maybe.map (List.length << .rackTiles << .playerRack )
                                                (getPlayer context.playerId game.gamePlayers)
                      }
                else -- goes home otherwise
                     { model
                     | dragOffsets = updatedOffsets
                     , dropoff = Just point
                     , rackDropoff = Nothing
                     }



        TrackTile (Just ((RackIndex i), MoveBy (dx,dy))) ->
            let updatedRackOffsets =
                    Dict.update i (Maybe.map (moveBy (dx,dy))) model.rackDragOffsets

                (rackX, rackY) = ((toFloat context.boardWidth) / 2, 100/2)

                (updatedX, updatedY) = Maybe.withDefault (0,0) (Dict.get i updatedRackOffsets)

                ((x',y') as boardPoint) =
                    (updatedX + (toFloat rackOffset), updatedY + globalOffset)
                        |> xyToBoard context
                        |> (\(bx,by) -> (bx, by + 1 )) -- added after empirical investigation

                rackOffset = -50 + 30 * i

                globalOffset = negate (toFloat context.boardHeight) / 2 - 50

            in noEffects <|

                {- if the tile only moves within the rack
                   then set the rack dropoff
                   else set the board dropoff
                -}

                if updatedX <= rackX && updatedY <= rackY
                then { model -- stay in the rack
                     | rackDragOffsets = updatedRackOffsets
                     , rackDropoff = Just i
                     , dropoff = Nothing
                     }
                else if (x' >= 0 && x' < 15 && y' >= 0 && y' < 15) -- new board point within bounds
                then { model
                     | rackDragOffsets = updatedRackOffsets
                     , dropoff = Just boardPoint
                     , rackDropoff = Nothing
                     }
                else -- out of bounds
                     { model
                     | rackDragOffsets = updatedRackOffsets
                     , rackDropoff = Just i
                     , dropoff = Nothing
                     }



        TrackTile (Just ((BoardIndex point), Release)) ->
         let squareOccupied = Maybe.isJust
                                ( model.dropoff
                                    `Maybe.andThen` \dropoffPoint ->
                                        Dict.get dropoffPoint game.gameBoard.contents
                                    `Maybe.andThen` .tile
                                )
             -- favor Points over Ints, somewhat arbitrarily
             index : Maybe TileIndex
             index =
                 Maybe.or
                    (Maybe.map BoardIndex model.dropoff)
                    (Maybe.map RackIndex model.rackDropoff)

         in case index of
                 -- Board to board
                 Just (BoardIndex dropoffPoint) ->
                     let model' =
                           { model
                           | dragOffsets =
                               Dict.remove point model.dragOffsets
                           , dropoff = Nothing
                           , boardOrigins = -- only put it in if it was previously there
                               if Set.member point model.boardOrigins
                               then Set.remove point model.boardOrigins
                                         |> Set.insert dropoffPoint
                               else model.boardOrigins
                           , game =
                               { game
                               | gameBoard = Game.Board <|
                                   if squareOccupied
                                   then game.gameBoard.contents -- don't do anything
                                   else ( game.gameBoard.contents

                                              -- evict the tile from its old home
                                             |> Dict.update point (Maybe.map (\s -> {s | tile = Nothing }))

                                             -- dig through a couple maybe layers to get a function that puts the
                                             -- moved tile in its place. If anything fails, apply
                                             -- the identity funciton instead
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
                    in  ( model'
                        , if isYourTurn context.playerId model'
                            then sendMessage context model' Game.ValidityCheck
                            else Effects.none
                        )
                 -- Board to rack
                 Just (RackIndex i) ->
                    let maybeTile : Maybe Game.Tile
                        maybeTile =
                            Dict.get point game.gameBoard.contents
                                `Maybe.andThen` .tile

                        model' =
                            { model
                            | dragOffsets =
                                Dict.remove point model.dragOffsets
                            , rackDropoff = Nothing
                            , boardOrigins =
                                Set.remove point model.boardOrigins
                            , game =
                                { game
                                | gameBoard = -- old board with the tile in question removed
                                    game.gameBoard.contents
                                        |> Dict.update point (Maybe.map (\s -> {s | tile = Nothing }))
                                        |> Game.Board
                                , gamePlayers = -- old rack with the tile tacked on to the back
                                    game.gamePlayers
                                        |> List.map
                                            ( \player ->
                                                if player.playerId == Game.playerIdToInt (context.playerId)
                                                then { player
                                                     | playerRack =
                                                        Game.Rack
                                                            ( player.playerRack.rackTiles ++
                                                               (Maybe.mapDefault [] List.singleton maybeTile)
                                                            )

                                                     }
                                                else player
                                            )

                                }
                            }
                    in ( model'
                       , if isYourTurn context.playerId model'
                            then sendMessage context model' Game.ValidityCheck
                            else Effects.none
                       )

                 Nothing -> noEffects model

        TrackTile (Just ((RackIndex i), Release)) ->
            let index : Maybe TileIndex
                index =
                    Maybe.or
                        (Maybe.map BoardIndex model.dropoff)
                        (Maybe.map RackIndex model.rackDropoff)

            in case index of
                  Just (BoardIndex dropoffPoint) -> -- rack to board
                        let squareOccupied =
                             (Dict.get dropoffPoint game.gameBoard.contents
                                `Maybe.andThen` .tile
                             ) |> Maybe.isJust

                            model' =
                                { model
                                | rackDragOffsets =
                                    Dict.remove i model.rackDragOffsets
                                , dropoff = Nothing
                                , boardOrigins = Set.insert dropoffPoint model.boardOrigins
                                , game =
                                    if squareOccupied
                                    then game
                                    else
                                        { game
                                        | gamePlayers = -- remove the tile from the rack
                                            game.gamePlayers
                                                |> List.map
                                                    ( \player ->
                                                        if player.playerId == Game.playerIdToInt (context.playerId)
                                                        then { player
                                                             | playerRack =
                                                                Game.Rack
                                                                    (remove i player.playerRack.rackTiles )
                                                             }
                                                        else player
                                                    )
                                        , gameBoard = -- add tile to board at point
                                            let maybeTile = -- the tile, if you can get it out of the list
                                                 Game.getPlayer context.playerId game.gamePlayers
                                                    `Maybe.andThen`
                                                        \player -> List.getAt player.playerRack.rackTiles i

                                            in game.gameBoard.contents
                                                    |> Dict.update dropoffPoint
                                                        (Maybe.map (\square -> {square | tile = maybeTile}))
                                                    |> Game.Board
                                        }
                                }

                        in ( model'
                           , if isYourTurn context.playerId model'
                             then sendMessage context model' Game.ValidityCheck
                             else Effects.none
                           )

                  Just (RackIndex i) ->  -- rack to rack
                      { model
                      | rackDragOffsets =
                          Dict.remove i model.rackDragOffsets
                      , rackDropoff = Nothing
                      } |> noEffects

                  Nothing -> noEffects model

        TrackTile Nothing ->
            noEffects model


sendMessage : Context -> Game.Model -> Game.MessageType -> Effects Action
sendMessage context model msgType =
    Game.Message msgType model.initialGameState (computeWordPut model)
        |> Game.encodeMessage
        |> Signal.send context.moveAddress
        |> Task.map (\_ -> NoOp)
        |> Effects.task


computeWordPut : Game.Model -> Game.WordPut
computeWordPut {game, boardOrigins} =
    boardOrigins
        |> Set.toList
        |> List.map
            ( \p -> (p, Dict.get p game.gameBoard.contents
                            `Maybe.andThen` .tile
                            |> Maybe.withDefault (Game.Tile Game.A 0)
                    )
            )
        |> List.map (\(p,t) -> Game.LetterTilePut t p) -- TODO figue out how to handle BlankTilePut
        |> Game.WordPut


remove : Int -> List a -> List a
remove i xs =
    List.take i xs ++ List.drop (i + 1) xs


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