module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (on, keyCode, onInput)
import Html.Attributes exposing (..)
import Json.Decode as Json
import Debug as Debug


type alias Todo =
    { title : String
    , completed : Bool
    , editing : Bool
    }


type FilterState
    = All
    | Active
    | Completed


type alias Model =
    { todos : List Todo
    , todo : Todo
    , filter : FilterState
    }


type Msg
    = Add
    | Complete Todo
    | Delete Todo
    | Filter FilterState
    | Change String


newTodo : Todo
newTodo =
    { title = ""
    , completed = False
    , editing = False
    }


initialModel : Model
initialModel =
    { todos =
        [ { title = "My todo"
          , completed = False
          , editing = False
          }
        ]
    , todo = newTodo
    , filter = All
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        Add ->
            { model
                | todos = model.todo :: model.todos
                , todo = newTodo
            }

        Complete todo ->
            model

        Delete todo ->
            model

        Filter filterState ->
            model

        Change value ->
            let
                todo =
                    model.todo

                updatedTodo =
                    { todo | title = value }
            in
                { model | todo = updatedTodo }


onEnter : Msg -> Attribute Msg
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                msg |> Debug.log "Msg" |> Json.succeed
            else
                Json.fail "not the right keycode"
    in
        on "keydown" (keyCode |> Json.andThen isEnter)


view : Model -> Html Msg
view model =
    section [ class "todoapp" ]
        [ header [ class "header" ]
            [ h1 [] [ text "todos" ]
            , input
                [ class "new-todo"
                , placeholder "What needs to be done?"
                , value model.todo.title
                , autofocus True
                , onEnter Add
                , onInput Change
                ]
                []
            ]
        , section [ class "main" ]
            [ ul [ class "todo-list" ]
                (List.map todoView model.todos)
            ]
        ]


todoView : Todo -> Html Msg
todoView todo =
    li [ classList [ ( "completed", todo.completed ) ] ]
        [ div [ class "view" ]
            [ input [ class "toggle", type_ "checkbox", checked todo.completed ] []
            , label [] [ text todo.title ]
            , button [ class "destroy" ] []
            ]
        ]


main =
    Html.beginnerProgram
        { model = initialModel
        , view = view
        , update = update
        }
