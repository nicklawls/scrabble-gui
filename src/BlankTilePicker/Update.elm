module BlankTilePicker.Update where

import BlankTilePicker.Model exposing (Model, PickerState(..))
import Effects exposing (Effects)
import Letter exposing (Letter)


type Action
    = NoOp
    | Open
    | SetChoice Letter
    | Close


noFx : a -> (a, Effects b)
noFx a = (a, Effects.none)


update : Action -> Model -> (Model, Effects Action)
update action model =
    case Debug.log "action" action of
        NoOp ->
            noFx model

        Open ->
            noFx { model | pickerState = Picking }

        SetChoice letter ->
            noFx { model
                 | letterChoice = Just letter
                 , pickerState = Idle
                 }

        Close ->
            noFx { model | pickerState = Idle} -- TODO may have to clear the choice here or on open