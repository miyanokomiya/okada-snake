module Main exposing (main)

import Array exposing (Array)
import Browser
import Browser.Events exposing (onKeyDown)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Json.Decode as Decode
import String.Interpolate exposing (interpolate)
import Svg
import Svg.Attributes


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
      , player =
            [ { position = ( 0, cellCount - 4 ), okada = Oka }
            , { position = ( 0, cellCount - 3 ), okada = Da }
            , { position = ( 0, cellCount - 2 ), okada = Oka }
            , { position = ( 0, cellCount - 1 ), okada = Da }
            ]
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
                            ( -1, 0 )

                        Right ->
                            ( 1, 0 )

                        Up ->
                            ( 0, -1 )

                        Down ->
                            ( 0, 1 )

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
movePlayer ( dx, dy ) player =
    Array.get 0 (Array.fromList player)
        |> Maybe.map
            (\top ->
                let
                    ( topX, topY ) =
                        top.position

                    nextTopX =
                        Basics.min (cellCount - 1) (Basics.max 0 (topX + dx))

                    nextTopY =
                        Basics.min (cellCount - 1) (Basics.max 0 (topY + dy))
                in
                if List.member ( nextTopX, nextTopY ) (List.map (\snakeCell -> snakeCell.position) player) then
                    player

                else
                    List.indexedMap
                        (\i snakeCell ->
                            if i == 0 then
                                { snakeCell | position = ( nextTopX, nextTopY ) }

                            else
                                { snakeCell
                                    | position =
                                        Array.get (i - 1) (Array.fromList player)
                                            |> Maybe.map (\c -> c.position)
                                            |> Maybe.withDefault snakeCell.position
                                }
                        )
                        player
            )
        |> Maybe.withDefault player



-- VIEW


cellCount =
    10


cellSize =
    40


view : Model -> Html Msg
view model =
    div
        [ style "padding" "1rem"
        , style "display" "flex"
        , style "flex-direction" "column"
        , style "align-items" "center"
        ]
        [ div []
            [ viewField model
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
                [ viewButton Down "Down"
                ]
            ]
        ]


viewField : Model -> Html Msg
viewField model =
    Svg.svg
        [ Svg.Attributes.width "400"
        , Svg.Attributes.height "400"
        , Svg.Attributes.viewBox (interpolate "0 0 {0} {1}" [ String.fromInt (cellCount * cellSize), String.fromInt (cellCount * cellSize) ])
        , style "border" "1px solid black"
        ]
    <|
        Array.toList (Array.indexedMap (\y row -> viewRow row y) model.field)
            ++ [ viewPlayer model.player ]


viewRow : Row -> Int -> Html Msg
viewRow row y =
    Svg.g [] <|
        Array.toList (Array.indexedMap (\x cell -> viewCell cell ( x, y )) row)


viewCell : Cell -> Position -> Html Msg
viewCell cell ( x, y ) =
    let
        gAttr =
            [ Svg.Attributes.transform (interpolate "translate({0}, {1})" [ String.fromInt (x * cellSize), String.fromInt (y * cellSize) ])
            ]

        rectAttr =
            [ Svg.Attributes.width (String.fromInt cellSize)
            , Svg.Attributes.height (String.fromInt cellSize)
            , Svg.Attributes.stroke "black"
            , Svg.Attributes.fill "none"
            ]
    in
    case cell of
        Block okada ->
            Svg.g
                gAttr
                [ Svg.rect
                    rectAttr
                    []
                , Svg.text_
                    [ Svg.Attributes.x (String.fromFloat (cellSize * 0.5))
                    , Svg.Attributes.y (String.fromFloat (cellSize * 0.55))
                    , Svg.Attributes.fontSize (String.fromFloat (cellSize * 0.6))
                    , Svg.Attributes.dominantBaseline "middle"
                    , Svg.Attributes.textAnchor "middle"
                    ]
                    [ Svg.text
                        (if okada == Oka then
                            "岡"

                         else
                            "田"
                        )
                    ]
                ]

        Empty ->
            Svg.g
                gAttr
                [ Svg.rect
                    rectAttr
                    []
                ]


viewPlayer : Snake -> Html Msg
viewPlayer player =
    Svg.g [] <|
        List.map viewSnakeCell player


viewSnakeCell : SnakeCell -> Html Msg
viewSnakeCell snakeCell =
    viewCell (Block snakeCell.okada) snakeCell.position


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
