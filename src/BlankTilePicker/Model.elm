module BlankTilePicker.Model where

import Letter exposing (Letter(..))
import Dict exposing (Dict)

-- TODO This seems shitty
-- re-declare type alias to avoid circular dependency
type alias Point = (Int,Int)

type alias Model =
    { pickerState   : PickerState
    , letterChoices : Dict Point (Maybe Letter)
    , currentPoint  : Maybe Point
    }


type PickerState
    = Idle
    | Picking


init : Model
init = Model Idle Dict.empty Nothing