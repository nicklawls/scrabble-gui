module BlankTilePicker.Update where

import BlankTilePicker.Model exposing (Model, PickerState(..), Point)
import Effects exposing (Effects)
import Letter exposing (Letter)
import Dict


type Action
    = NoOp
    | Open Point    -- open the picker to select a letter for the blank at Point
    | Clear
    | SetChoice Letter -- set the letter choice for the current point
    | Close


noFx : a -> (a, Effects b)
noFx a = (a, Effects.none)


update : Action -> Model -> (Model, Effects Action)
update action model =
    case Debug.log "action" action of
        NoOp ->
            noFx model

        Open point ->
            noFx { model
                 | pickerState = Picking
                 , letterChoices = Dict.remove point model.letterChoices
                 , currentPoint = Just point
                 }

        Clear ->
            noFx { model
                 | letterChoices = Dict.empty
                 , pickerState = Idle
                 , currentPoint = Nothing
                 }

        SetChoice letter ->
            noFx <| case model.currentPoint of
                        Just point -> { model
                                      | letterChoices =
                                          Dict.insert point letter model.letterChoices
                                      , pickerState = Idle
                                      , currentPoint = Nothing
                                      }
                        Nothing -> model -- TODO some error condition here

        Close ->
            noFx { model | pickerState = Idle} -- TODO may have to clear the choice here or on open