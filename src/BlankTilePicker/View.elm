module BlankTilePicker.View where

import BlankTilePicker.Model exposing (Model, PickerState(..))
import BlankTilePicker.Update exposing (Action(..))
import Signal exposing (Address)
import Html exposing (Html, Attribute)
import Html.Events
import Json.Decode
import Letter


debug : Bool
debug = False


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
                [ Html.select [ letterOnChange address ] <|
                    ( Letter.letters
                        |> List.map toString
                        |> List.map (\l -> Html.option [] [ Html.text l ])
                    )


                , Html.button [ Html.Events.onClick address Close ]
                    [ Html.text "Close" ]
                , if debug
                  then Html.div [] [Html.text (toString model.letterChoice)]
                  else Html.div [] []
                ]


letterOnChange : Address Action -> Attribute
letterOnChange address =
    Html.Events.on "change"
        ( Json.Decode.map SetChoice <|
            Json.Decode.customDecoder Html.Events.targetValue Letter.parseLetter
        )
        (Signal.message address)