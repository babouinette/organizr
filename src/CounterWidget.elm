module CounterWidget exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


type alias CounterModel =
    { value : Int }



-- UPDATE


type alias WidgetId =
    Int


type alias NewValue =
    Int


type Msg
    = UpdateValue CounterModel WidgetId NewValue
    | ResetValue CounterModel WidgetId
    | DeleteWidget WidgetId


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        UpdateValue counterModel widgetId newValue ->
            updateValue counterModel widgetId newValue model

        ResetValue counterModel widgetId ->
            updateValue counterModel widgetId 0 model

        DeleteWidget widgetId ->
            (List.take widgetId model ++ List.drop (widgetId + 1) model) ! [ Cmd.none ]


updateValue : CounterModel -> WidgetId -> NewValue -> Model -> ( Model, Cmd Msg )
updateValue counterModel widgetId newValue model =
    let
        nextModel =
            List.indexedMap
                (\index widget ->
                    if widgetId == index then
                        Counter { counterModel | value = newValue }
                    else
                        widget
                )
                model
    in
        nextModel ! [ Cmd.none ]



-- VIEW


view : Int -> CounterModel -> Html Msg
view widgetId counterModel =
    div [ class "column is-one-third" ]
        [ div [ class "card" ]
            [ header [ class "card-header" ]
                [ p [ class "card-header-title" ] [ text ("Counter number " ++ toString (widgetId)) ] ]
            , div [ class "card-content" ]
                [ div [ class "content has-text-centered" ]
                    [ div [] [ text (toString counterModel.value) ]
                    ]
                ]
            , footer [ class "card-footer" ]
                [ a [ class "card-footer-item", onClick <| UpdateValue counterModel widgetId (counterModel.value - 1) ] [ text "-" ]
                , a [ class "card-footer-item", onClick <| UpdateValue counterModel widgetId (counterModel.value + 1) ] [ text "+" ]
                , a [ class "card-footer-item", onClick <| ResetValue counterModel widgetId ] [ text "Reset" ]
                , a [ class "card-footer-item", onClick <| DeleteWidget widgetId ] [ text "Delete" ]
                ]
            ]
        ]
