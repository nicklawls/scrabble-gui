module BlankTilePicker.Update where

import BlankTilePicker.Model exposing (Model)
import Effects exposing (Effects)


type Action = Act | NoAct

update : Action -> Model -> (Model, Effects Action)
update = Debug.crash "foo"