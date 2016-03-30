module Scrabble.Update where


import Scrabble.Model as Scrabble exposing (Model, GameState(..))
import Effects exposing (Effects)
import Task exposing (Task)
import Game.Model exposing (PlayerId(..))
import Game.Update as Game
import Game.Encode as Game
import Signal exposing (Address)


type Action
    = NoOp
    | EditName String
    | SendName
    | SetId Game.Model.PlayerId
    | EditCommand String
    | SendMove
    | GameAction Game.Action


-- Result is elm's Either
-- Result e a = Err e | Ok a


-- contains the addresses to send name and moves to, provided by parent
type alias Context =
    { moveAddress : Address String
    , nameAddress : Address String
    }


update : Context -> Action -> Model -> (Model, Effects Action)
update context action model =
    case action of
        NoOp ->
            (model, Effects.none)

        EditName name ->
            ({ model | playerName = name}, Effects.none)

        SendName ->
            (model, sendName context model)


        SetId id ->
            ( { model | playerId = id, state = Waiting}
            , Effects.none
            )

        EditCommand command ->
            ({ model | command = command } , Effects.none)

        SendMove ->
            (model, sendMove context model)

        GameAction gameAction ->
            let (game, fx) = Game.update (Game.Context model.playerId 500 500) gameAction model.game
            in ( { model
                 | game = game
                 -- only change the view if you have an id
                 , state = case model.playerId of
                            Unassigned -> SignIn

                            _ -> Playing
                 }
               , Effects.map GameAction fx
               )


sendMove : Context -> Model -> Effects Action
sendMove context model =
    (model.game.game, model.command)
        |> Game.encodeGameAndMove
        |> Signal.send context.moveAddress
        |> Task.map (\_ -> NoOp)
        |> Effects.task


{- The above is the Elm-y way of doing it, making heavy use of flipped
   function application; (|>) : a -> (a -> b) -> b.
   You can achieve a more haskelly style with (<|), the equivalent of ($)
-}


sendName : Context -> Model -> Effects Action
sendName context model =
    model.playerName
        |> Signal.send context.nameAddress
        |> Task.map (\_ -> NoOp)
        |> Effects.task


init : (Model, Effects Action)
init =
    ( Scrabble.initialModel
    , Effects.task (Task.succeed NoOp)
    )