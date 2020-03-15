module Main exposing (main)

import Array exposing (Array)
import Browser
import Browser.Events exposing (onKeyDown)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Json.Decode as Decode


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { field : Field
    , steps : List Step
    , player : Snake
    }


type alias Field =
    Array Row


type alias Row =
    Array Cell


type Cell
    = Block Okada
    | Empty


type Okada
    = Oka
    | Da


type alias SnakeCell =
    { position : Position
    , okada : Okada
    }


type alias Snake =
    List SnakeCell


type alias Position =
    ( Int, Int )


type Step
    = Left
    | Right
    | Up
    | Down
    | Other


init : () -> ( Model, Cmd Msg )
init _ =
    ( { field =
            Array.repeat 10 <| Array.repeat 10 <| Empty
      , player = [ { position = ( 0, 0 ), okada = Oka } ]
      , steps = []
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = Change Step


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Change step ->
            let
                position =
                    case step of
                        Left ->
                            ( 0, -1 )

                        Right ->
                            ( 0, 1 )

                        Up ->
                            ( -1, 0 )

                        Down ->
                            ( 1, 0 )

                        Other ->
                            ( 0, 0 )
            in
            ( { model
                | player = movePlayer position model.player
                , steps = List.append model.steps [ step ]
              }
            , Cmd.none
            )


movePlayer : Position -> Snake -> Snake
movePlayer ( x, y ) player =
    List.indexedMap
        (\i snakeCell ->
            if i == 0 then
                { snakeCell | position = ( x, y ) }

            else
                { snakeCell
                    | position =
                        Array.get (i - 1) (Array.fromList player)
                            |> Maybe.map (\c -> c.position)
                            |> Maybe.withDefault snakeCell.position
                }
        )
        player



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ style "padding" "1rem"
        , style "display" "flex"
        , style "flex-direction" "column"
        , style "align-items" "center"
        ]
        [ div []
            [ viewBoard model.field
            ]
        , div
            [ style "margin" "0.5rem"
            , style "display" "flex"
            , style "flex-direction" "column"
            , style "align-items" "center"
            ]
            [ div []
                [ viewButton Up "Up"
                ]
            , div []
                [ viewButton Left "Left"
                , viewButton Right "Right"
                ]
            , div []
                [ viewButton Down "D"
                ]
            ]
        ]


viewBoard : Field -> Html Msg
viewBoard field =
    div
        [ style "display" "inline-flex"
        , style "flex-direction" "column"
        , style "border" "1px solid black"
        ]
    <|
        Array.toList (Array.map viewRow field)


viewRow : Row -> Html Msg
viewRow row =
    div [ style "display" "flex" ] <|
        Array.toList (Array.map viewCell row)


viewCell : Cell -> Html Msg
viewCell cell =
    let
        boxStyle =
            [ style "width" "2rem"
            , style "height" "2rem"
            , style "border" "1px solid black"
            ]
    in
    case cell of
        Block okada ->
            span
                boxStyle
                [ text
                    (if okada == Oka then
                        "岡"

                     else
                        "田"
                    )
                ]

        Empty ->
            span boxStyle []


viewButton : Step -> String -> Html Msg
viewButton step label =
    let
        buttonStyle =
            [ style "margin" "0 0.2rem 0.1rem"
            , style "width" "4rem"
            , style "height" "1.5rem"
            , style "border" "1px solid black"
            ]
    in
    button (List.append buttonStyle [ onClick (Change step) ]) [ text label ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ onKeyDown (Decode.map Change keyDecoder)
        ]


keyDecoder : Decode.Decoder Step
keyDecoder =
    Decode.map toDirection (Decode.field "key" Decode.string)


toDirection : String -> Step
toDirection string =
    case string of
        "ArrowLeft" ->
            Left

        "ArrowRight" ->
            Right

        "ArrowUp" ->
            Up

        "ArrowDown" ->
            Down

        _ ->
            Other
