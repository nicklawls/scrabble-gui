module BlankTilePicker.View where

import BlankTilePicker.Model exposing (Model, PickerState(..), Point)
import BlankTilePicker.Update exposing (Action(..))
import Dict
import Signal exposing (Address)
import Html exposing (Html, Attribute)
import Html.Events exposing (on, onClick)
import Html.Attributes exposing (selected)
import Json.Decode
import Maybe.Extra as Maybe
import Letter


-- debug : Bool
-- debug = True


view : Address Action -> Model -> Html
view address model =
    case model.pickerState of
        Idle ->
            Html.div []  [] -- <|
                -- if debug
                -- then [ Html.button [onClick address (Open context.point)] [Html.text "Open"] ]
                -- else []

        Picking ->
            let point = Maybe.withDefault (Debug.log "is there a sane default here?" (0,0)) model.currentPoint
            in Html.div []
                [ Html.select [ letterOnChange address point ] <|
                    [ Html.option [] [Html.text "--" ] ] ++
                    ( Letter.letters
                        |> List.map
                            ( \l -> Html.option
                                        [ selected
                                            ( Dict.get point model.letterChoices
                                                |> Maybe.join -- The point wans't in the dict
                                                |> Maybe.mapDefault False (\x -> x == l)
                                            )


                                        ]
                                        [ Html.text (toString l) ]
                            )
                    )


                , Html.button [ Html.Events.onClick address Close ]
                    [ Html.text "Close" ]
                -- , if debug
                --   then Html.div [] [Html.text (toString model.letterChoices)]
                --   else Html.div [] []
                ]


letterOnChange : Address Action -> Point -> Attribute
letterOnChange address point  =
    -- TODO may want to prompt the user when they choose "--"
    on "change"
        ( Json.Decode.map (SetChoice point) <|
            Json.Decode.customDecoder Html.Events.targetValue Letter.parseLetter
        )
        (Signal.message address)