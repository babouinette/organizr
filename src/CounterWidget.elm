module CounterWidget exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


type alias Model =
    Int


initialModel =
    0



-- UPDATE


type Msg
    = Increment
    | Decrement
    | Reset
    | Delete


type OutMsg
    = OutNoOp
    | DeleteWidget


update : Msg -> Model -> Model
update message model =
    case message of
        Increment ->
            model + 1

        Decrement ->
            model - 1

        Reset ->
            0

        Delete ->
            model


updateOutMsg : Msg -> Model -> OutMsg
updateOutMsg msg model =
    case msg of
        Delete ->
            DeleteWidget

        _ ->
            OutNoOp



-- VIEW


view : Int -> Model -> Html Msg
view widgetId model =
    div [ class "column is-one-third" ]
        [ div [ class "card" ]
            [ header [ class "card-header" ]
                [ p [ class "card-header-title" ] [ text ("Counter " ++ toString (widgetId)) ] ]
            , div [ class "card-content" ]
                [ div [ class "content has-text-centered" ]
                    [ div [] [ text (toString model) ]
                    ]
                ]
            , footer [ class "card-footer" ]
                [ a [ class "card-footer-item", onClick Decrement ] [ text "-" ]
                , a [ class "card-footer-item", onClick Increment ] [ text "+" ]
                , a [ class "card-footer-item", onClick Reset ] [ text "Reset" ]
                , a [ class "card-footer-item", onClick Delete ] [ text "Delete" ]
                ]
            ]
        ]
