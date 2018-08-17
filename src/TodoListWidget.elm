module TodoListWidget exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


type alias Model =
    { todos : List Todo
    , currentTodo : String
    , uid : Int
    }


type alias Todo =
    { content : String
    , completed : Bool
    , editing : Bool
    , id : Int
    }


newTodo : String -> Int -> Todo
newTodo content id =
    Todo content False False id


initialModel =
    Model [ newTodo "New todo" 0 ] "" 1



-- UPDATE


type Msg
    = UpdateField String
    | AddTodo
    | DeleteTodo Int
    | Delete
    | ToggleCompleted Int


type OutMsg
    = OutNoOp
    | DeleteWidget


update : Msg -> Model -> Model
update msg model =
    case Debug.log "msg" msg of
        UpdateField str ->
            { model | currentTodo = str }

        AddTodo ->
            let
                emptyTodo =
                    String.isEmpty model.currentTodo
            in
                { model
                    | uid =
                        if emptyTodo then
                            model.uid
                        else
                            model.uid + 1
                    , currentTodo = ""
                    , todos =
                        if emptyTodo then
                            model.todos
                        else
                            List.append model.todos [ newTodo model.currentTodo model.uid ]
                }

        Delete ->
            model

        DeleteTodo todoId ->
            { model | todos = List.filter (\t -> t.id /= todoId) model.todos }

        ToggleCompleted todoId ->
            let
                updateTodo todo =
                    if todo.id == todoId then
                        { todo | completed = not todo.completed }
                    else
                        todo
            in
                { model | todos = List.map updateTodo model.todos }


updateOutMsg : Msg -> Model -> OutMsg
updateOutMsg msg model =
    case msg of
        Delete ->
            DeleteWidget

        _ ->
            OutNoOp



-- VIEW


todoView : Int -> Todo -> Html Msg
todoView todoId todo =
    tr [ onClick (ToggleCompleted todo.id), classList [ ( "is-selected", todo.completed ) ] ]
        [ th [] [ text (toString (todoId + 1)) ]
        , td [] [ text (toString (todo.id)) ]
        , td [] [ text todo.content ]
        , td []
            [ button [ class "button", onClick (DeleteTodo todo.id) ]
                [ text "X" ]
            ]
        ]


inputTodo : Model -> Html Msg
inputTodo model =
    div [ class "field" ]
        [ label [ class "label" ] [ text "Create a Todo" ]
        , div [ class "control" ] [ input [ class "input", placeholder "New todo", onInput UpdateField, value model.currentTodo ] [] ]
        ]


view : Int -> Model -> Html Msg
view widgetId model =
    div [ class "column is-one-third" ]
        [ div [ class "card" ]
            [ header [ class "card-header" ]
                [ p [ class "card-header-title" ] [ text ("TodoList number " ++ toString (widgetId)) ] ]
            , div [ class "card-content" ]
                [ div [ class "content has-text-centered" ]
                    [ inputTodo model
                    , table [ class "table is-hoverable" ]
                        [ thead []
                            [ tr []
                                [ th [] [ text "" ]
                                , th [] [ text "uid" ]
                                , th [] [ text "Description" ]
                                , th [] [ text "Actions" ]
                                ]
                            ]
                        , tbody []
                            (List.indexedMap todoView model.todos)
                        ]
                    ]
                ]
            , footer [ class "card-footer" ]
                [ a [ class "card-footer-item", onClick AddTodo ] [ text "Add todo" ]
                , a [ class "card-footer-item", onClick Delete ] [ text "Delete list" ]
                ]
            ]
        ]
