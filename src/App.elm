module App exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Attributes.Aria exposing (..)
import Html.Events exposing (..)
import CounterWidget
import TodoListWidget


type Widget
    = Counter CounterWidget.Model
    | TodoList TodoListWidget.Model


type alias Model =
    List Widget


init : ( Model, Cmd Msg )
init =
    let
        initialModel =
            [ Counter CounterWidget.initialModel, TodoList (TodoListWidget.initialModel) ]
    in
        ( initialModel, Cmd.none )



-- UPDATE


type alias WidgetId =
    Int


type alias NewValue =
    Int


type Msg
    = AddWidget Widget
    | CounterMsg WidgetId CounterWidget.Msg
    | TodoListMsg WidgetId TodoListWidget.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    (case Debug.log "msg" msg of
        CounterMsg widgetId msg_ ->
            let
                updatedModel =
                    List.indexedMap
                        (\index widget ->
                            if widgetId == index then
                                case Debug.log "widget" widget of
                                    Counter model ->
                                        Counter (CounterWidget.update msg_ model)

                                    _ ->
                                        Debug.crash "Thet ype of the widget should be COunter because the message is sent from a COunter Widget"
                            else
                                widget
                        )
                        model
            in
                updatedModel ! [ Cmd.none ]

        TodoListMsg widgetId msg_ ->
            let
                updatedModel =
                    List.indexedMap
                        (\index widget ->
                            if widgetId == index then
                                case Debug.log "widget" widget of
                                    TodoList model ->
                                        TodoList (TodoListWidget.update msg_ model)

                                    _ ->
                                        widget
                            else
                                widget
                        )
                        model
            in
                updatedModel ! [ Cmd.none ]

        AddWidget widget ->
            case widget of
                Counter counterModel ->
                    (List.append model [ Counter counterModel ]) ! [ Cmd.none ]

                TodoList todoListModel ->
                    (List.append model [ TodoList todoListModel ]) ! [ Cmd.none ]
    )
        |> updateOutMsg msg


updateOutMsg : Msg -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
updateOutMsg msg ( model, command ) =
    case msg of
        CounterMsg widgetId msg_ ->
            case (CounterWidget.updateOutMsg msg_ CounterWidget.initialModel) of
                CounterWidget.OutNoOp ->
                    ( model, command )

                CounterWidget.DeleteWidget ->
                    (List.take widgetId model ++ List.drop (widgetId + 1) model) ! [ command ]

        TodoListMsg widgetId msg_ ->
            case (TodoListWidget.updateOutMsg msg_ TodoListWidget.initialModel) of
                TodoListWidget.OutNoOp ->
                    ( model, command )

                TodoListWidget.DeleteWidget ->
                    (List.take widgetId model ++ List.drop (widgetId + 1) model) ! [ command ]

        _ ->
            ( model, command )



-- VIEW


navbar =
    nav [ class "navbar is-warning", ariaLabel "main navigation" ]
        [ div [ class "container" ]
            [ div [ class "navbar-brand" ]
                [ a [ class "navbar-item", href "https://bulma.io" ]
                    [ img [ src "https://bulma.io/images/bulma-logo.png", alt "Bulma: a modern CSS framework based on Flexbox", width 112, height 28 ] []
                    ]
                ]
            , a [ role "button", class "navbar-burger", ariaLabel "menu", ariaExpanded "false" ]
                [ span [ ariaHidden True ] []
                , span [ ariaHidden True ] []
                , span [ ariaHidden True ] []
                ]
            ]
        ]


view : Model -> Html Msg
view model =
    div []
        [ navbar
        , div
            [ class "container section" ]
            [ div [ class "columns is-multiline", style [ ( "height", "800px" ) ] ] <| List.indexedMap displayWidget model
            , button [ class "button", onClick (AddWidget (Counter CounterWidget.initialModel)) ] [ text "Add a counter!" ]
            , button [ class "button", onClick (AddWidget (TodoList (TodoListWidget.initialModel))) ] [ text "Add todo list" ]
            ]
        ]


displayWidget : WidgetId -> Widget -> Html Msg
displayWidget widgetId widgetType =
    case widgetType of
        Counter counterModel ->
            Html.map (CounterMsg widgetId) <| CounterWidget.view widgetId counterModel

        TodoList todoListModel ->
            Html.map (TodoListMsg widgetId) <| TodoListWidget.view widgetId todoListModel
