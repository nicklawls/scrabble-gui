module BlankTilePicker.View where

import BlankTilePicker.Model exposing (Model, PickerState(..))
import BlankTilePicker.Update exposing (Action(..))
import Signal exposing (Address)
import Html exposing (Html, Attribute)
import Html.Events
import Json.Decode
import Letter


debug : Bool
debug = True


view : Address Action -> Model -> Html
view address model =
    case model.pickerState of
        Idle ->
            Html.div [] <|
                if debug
                then [ Html.button [Html.Events.onClick address Open] [Html.text "Open"] ]
                else []

        Picking ->
            Html.div []
                [ Html.select [ onChange address ] <|
                    List.map (\l -> Html.option [] [Html.text (toString l)]) Letter.letters


                , Html.button [ Html.Events.onClick address Close ]
                    [ Html.text "Close" ]
                , if debug
                  then Html.div [] [Html.text (toString model.letterChoice)]
                  else Html.div [] []
                ]


onChange : Address Action -> Attribute
onChange address =
    Html.Events.on "change"
        (Json.Decode.customDecoder Html.Events.targetValue Letter.parseLetter
            |> Json.Decode.map SetChoice
        )
        (Signal.message address)