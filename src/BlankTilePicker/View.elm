module BlankTilePicker.View where

import BlankTilePicker.Model exposing (Model, PickerState(..))
import BlankTilePicker.Update exposing (Action(..))
import Signal exposing (Address)
import Html exposing (Html, Attribute)
import Html.Events exposing (on, onClick)
import Html.Attributes exposing (selected)
import Json.Decode
import Maybe.Extra as Maybe
import Letter


debug : Bool
debug = True


view : Address Action -> Model -> Html
view address model =
    case model.pickerState of
        Idle ->
            Html.div [] <|
                if debug
                then [ Html.button [onClick address Open] [Html.text "Open"] ]
                else []

        Picking ->
            Html.div []
                [ Html.select [ letterOnChange address ] <|
                    [ Html.option [] [Html.text "--" ] ] ++
                    ( Letter.letters
                        |> List.map
                            ( \l -> Html.option
                                        [ selected <|
                                            Maybe.mapDefault False (\x -> x == l)
                                                model.letterChoice
                                        ]
                                        [ Html.text (toString l) ]
                            )
                    )


                , Html.button [ Html.Events.onClick address Close ]
                    [ Html.text "Close" ]
                , if debug
                  then Html.div [] [Html.text (toString model.letterChoice)]
                  else Html.div [] []
                ]


letterOnChange : Address Action -> Attribute
letterOnChange address =
    -- TODO may want to prompt the user when they choose "--"
    on "change"
        ( Json.Decode.map SetChoice <|
            Json.Decode.customDecoder Html.Events.targetValue Letter.parseLetter
        )
        (Signal.message address)