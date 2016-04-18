module BlankTilePicker.Model where
import Letter exposing (Letter(..))

type alias Model =
    { pickerState : PickerState
    , letterChoice: Maybe Letter
    }


type PickerState
    = Idle
    | Picking


init : Model
init = Model Idle Nothing