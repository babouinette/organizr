module App exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


type alias CounterModel =
    { value : Int }


type alias TodoListModel =
    { todos : List String
    , currentTodo : String
    }


type Widget
    = Counter CounterModel
    | TodoList TodoListModel


type alias Model =
    List Widget


init : ( Model, Cmd Msg )
init =
    let
        initialModel =
            [ Counter (CounterModel 5), TodoList (TodoListModel [ "Hello", "World" ] "Current"), Counter (CounterModel 6) ]
    in
        ( initialModel, Cmd.none )



-- UPDATE


type alias WidgetId =
    Int


type alias NewValue =
    Int


type Msg
    = AddCounter
    | AddTodoList
    | UpdateValue CounterModel WidgetId NewValue
    | ResetValue CounterModel WidgetId
    | DeleteWidget WidgetId
    | UpdateField TodoListModel WidgetId String
    | AddTodo TodoListModel WidgetId


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        AddCounter ->
            (List.append model [ Counter (CounterModel 1) ]) ! [ Cmd.none ]

        AddTodoList ->
            (List.append model [ TodoList (TodoListModel [ "new" ] "list") ]) ! [ Cmd.none ]

        UpdateValue counterModel widgetId newValue ->
            updateValue counterModel widgetId newValue model

        ResetValue counterModel widgetId ->
            updateValue counterModel widgetId 0 model

        DeleteWidget widgetId ->
            (List.take widgetId model ++ List.drop (widgetId + 1) model) ! [ Cmd.none ]

        UpdateField todoListModel widgetId str ->
            let
                nextModel =
                    List.indexedMap
                        (\index widget ->
                            if widgetId == index then
                                TodoList { todoListModel | currentTodo = str }
                            else
                                widget
                        )
                        model
            in
                nextModel ! [ Cmd.none ]

        AddTodo todoListModel widgetId ->
            let
                nextModel =
                    List.indexedMap
                        (\index widget ->
                            if widgetId == index then
                                TodoList { todoListModel | todos = List.append todoListModel.todos [ todoListModel.currentTodo ] }
                            else
                                widget
                        )
                        model
            in
                nextModel ! [ Cmd.none ]


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


view : Model -> Html Msg
view model =
    div [ class "container section" ]
        [ div [ class "columns is-multiline" ] <| List.indexedMap displayWidget model
        , button [ class "button", onClick AddCounter ] [ text "Add a counter!" ]
        , button [ class "button", onClick AddTodoList ] [ text "Add todo list" ]
        ]


counterView : Int -> CounterModel -> Html Msg
counterView widgetId counterModel =
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


todoListView : Int -> TodoListModel -> Html Msg
todoListView widgetId todoListModel =
    let
        todoView todo =
            li [] [ text todo ]
    in
        div [ class "column is-one-third" ]
            [ div [ class "card" ]
                [ header [ class "card-header" ]
                    [ p [ class "card-header-title" ] [ text ("TodoList number " ++ toString (widgetId)) ] ]
                , div [ class "card-content" ]
                    [ div [ class "content has-text-centered" ]
                        [ ul [] (List.map todoView todoListModel.todos)
                        ]
                    , input [ placeholder "new todo", onInput (UpdateField todoListModel widgetId), value todoListModel.currentTodo ] []
                    ]
                , footer [ class "card-footer" ]
                    [ a [ class "card-footer-item", onClick (AddTodo todoListModel widgetId) ] [ text "Add todo" ]
                    , a [ class "card-footer-item" ] [ text "Toggle" ]
                    , a [ class "card-footer-item", onClick <| DeleteWidget widgetId ] [ text "Delete" ]
                    ]
                ]
            ]


displayWidget : Int -> Widget -> Html Msg
displayWidget widgetId widgetType =
    case widgetType of
        Counter counterModel ->
            counterView widgetId counterModel

        TodoList todoListModel ->
            todoListView widgetId todoListModel
