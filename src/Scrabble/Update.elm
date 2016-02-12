module Scrabble.Update where

import Scrabble.Model as Scrabble exposing (Model)
import Effects exposing (Effects)
import Task exposing (Task)
import Scrabble.Game as Scrabble exposing (Game)
import Signal exposing (Address)

type Action
    = NoOp
    | EditCommand String
    | SendMove
    | RecieveGame (Result String Game)


-- Result is elm's Either
-- Result e a = Err e | Ok a


type alias Context =
    { sendMoveAddress : Address String }


update : Context -> Action -> Model -> (Model, Effects Action)
update context action model =
    case action of
        NoOp ->
            (model, Effects.none)

        EditCommand command ->
            ( { model | command = command } , Effects.none )

        SendMove ->
            ( model, sendMove context model )

        RecieveGame (Ok game) ->
            ( { model | game = game }, Effects.none )

        RecieveGame (Err error) -> -- do nothing for now
            let log = Debug.log "game receipt error"
            in ( model, Effects.none)


sendMove : Context -> Model -> Effects Action
sendMove {sendMoveAddress} model =
    model.command
        |> Signal.send sendMoveAddress
        |> Task.map (\_ -> NoOp)
        |> Effects.task


{- The above is the Elm-y way of doing it, making heavy use of flipped
   function application; (|>) : a -> (a -> b) -> b.
   You can achieve a more haskelly style with (<|), the equivalent of ($)
-}


init : (Model, Effects Action)
init =
    ( Scrabble.initialModel
    , Effects.task (Task.succeed NoOp)
    )