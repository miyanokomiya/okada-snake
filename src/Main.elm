module Main exposing (main)

import Array
import Browser
import Browser.Events exposing (onKeyDown)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Json.Decode as Decode
import Model
import Random
import String.Interpolate exposing (interpolate)
import Svg
import Svg.Attributes


main : Program () Model.Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- UPDATE


init : () -> ( Model.Model, Cmd Msg )
init _ =
    ( { field =
            List.repeat 10 <| List.repeat 10 <| Model.Empty
      , player =
            ( { position = ( 0, Model.cellCount - 1 ), okada = Model.Oka, direction = Model.Up }
            , []
            )
      , steps = []
      }
    , Cmd.none
    )


type Msg
    = Change Model.Direction
    | GenerateBlock Model.Position


update : Msg -> Model.Model -> ( Model.Model, Cmd Msg )
update msg model =
    case msg of
        Change direction ->
            if direction == Model.Other then
                ( model, Cmd.none )

            else
                let
                    ( field, player ) =
                        Model.movePlayer direction ( model.field, model.player )
                in
                ( { model
                    | player = player
                    , field = field
                    , steps = List.append model.steps [ direction ]
                  }
                , Random.generate GenerateBlock (Random.pair (Random.int 0 (Model.cellCount - 1)) (Random.int 0 (Model.cellCount - 1)))
                )

        GenerateBlock pos ->
            let
                count =
                    List.length model.steps
            in
            if modBy 2 count == 1 then
                ( model, Cmd.none )

            else
                ( { model
                    | field =
                        Model.generateBlock ( model.field, model.player )
                            pos
                            (if modBy 4 (List.length model.steps) == 2 then
                                Model.Oka

                             else
                                Model.Da
                            )
                  }
                , Cmd.none
                )



-- VIEW


cellSize : Int
cellSize =
    40


cellSizeFloat : Float
cellSizeFloat =
    toFloat cellSize


view : Model.Model -> Html Msg
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
            , style "align-items" "flex-start"
            , style "justify-content" "space-between"
            ]
            [ div
                [ style "margin" "0 2rem"
                ]
                [ viewScore model.player
                ]
            , div
                [ style "margin" "0 2rem"
                , style "display" "flex"
                , style "flex-direction" "column"
                , style "align-items" "center"
                ]
                [ div []
                    [ viewButton Model.Up "Up"
                    ]
                , div []
                    [ viewButton Model.Left "Left"
                    , viewButton Model.Right "Right"
                    ]
                , div []
                    [ viewButton Model.Down "Down"
                    ]
                ]
            ]
        ]


viewField : Model.Model -> Html Msg
viewField model =
    Svg.svg
        [ Svg.Attributes.width "400"
        , Svg.Attributes.height "400"
        , Svg.Attributes.viewBox (interpolate "0 0 {0} {1}" [ String.fromInt (Model.cellCount * cellSize), String.fromInt (Model.cellCount * cellSize) ])
        , style "border" "1px solid black"
        ]
    <|
        List.indexedMap (\y row -> viewRow row y) model.field
            ++ [ viewPlayer model.player ]


viewRow : Model.Row -> Int -> Html Msg
viewRow row y =
    Svg.g [] <|
        List.indexedMap (\x cell -> viewCell cell ( x, y )) row


viewCell : Model.Cell -> Model.Position -> Html Msg
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
        Model.Block okada ->
            Svg.g
                gAttr
                [ Svg.rect
                    rectAttr
                    []
                , Svg.text_
                    [ Svg.Attributes.x (String.fromFloat (cellSizeFloat * 0.5))
                    , Svg.Attributes.y (String.fromFloat (cellSizeFloat * 0.55))
                    , Svg.Attributes.fontSize (String.fromFloat (cellSizeFloat * 0.6))
                    , Svg.Attributes.dominantBaseline "middle"
                    , Svg.Attributes.textAnchor "middle"
                    ]
                    [ Svg.text
                        (if okada == Model.Oka then
                            "岡"

                         else
                            "田"
                        )
                    ]
                ]

        Model.Empty ->
            Svg.g
                gAttr
                [ Svg.rect
                    rectAttr
                    []
                ]


viewPlayer : Model.Snake -> Html Msg
viewPlayer player =
    let
        ( head, body ) =
            player

        cells =
            Model.snakeCells player
    in
    Svg.g []
        (List.append
            (List.append
                (List.indexedMap
                    (\i child ->
                        let
                            mayBeBefore =
                                Array.get i (Array.fromList cells)
                        in
                        mayBeBefore
                            |> Maybe.map
                                (\parent -> viewSnakeLine parent child)
                            |> Maybe.withDefault (Svg.g [] [])
                    )
                    body
                )
                [ viewSnakeCell 0 head ]
            )
            (List.indexedMap (\i c -> viewSnakeCell (i + 1) c) body)
        )


viewSnakeLine : Model.SnakeCell -> Model.SnakeCell -> Html Msg
viewSnakeLine parent child =
    let
        ( parentX, parentY ) =
            parent.position

        ( childX, childY ) =
            child.position

        ( dx, dy ) =
            ( cellSizeFloat * 0.3, cellSizeFloat * 0.33 )

        ( cx, cy ) =
            ( (toFloat (parentX + childX) / 2 + 0.5) * cellSizeFloat, (toFloat (parentY + childY) / 2 + 0.5) * cellSizeFloat )

        gAttr =
            [ Svg.Attributes.transform
                (interpolate
                    "rotate({0}, {1}, {2})"
                    [ if parentX == childX then
                        "0"

                      else
                        "90"
                    , String.fromFloat cx
                    , String.fromFloat cy
                    ]
                )
            ]

        pathAttr =
            [ Svg.Attributes.d
                (interpolate
                    "M {0} {1} Q {2} {3} {4} {5} L {6} {7} Q {8} {9} {10} {11}"
                    [ String.fromFloat (cx + dx)
                    , String.fromFloat (cy + dy)
                    , String.fromFloat (cx - dx * 0.1)
                    , String.fromFloat cy
                    , String.fromFloat (cx + dx)
                    , String.fromFloat (cy - dy)
                    , String.fromFloat (cx - dx)
                    , String.fromFloat (cy - dy)
                    , String.fromFloat (cx + dx * 0.1)
                    , String.fromFloat cy
                    , String.fromFloat (cx - dx)
                    , String.fromFloat (cy + dy)
                    ]
                )
            , Svg.Attributes.stroke "black"
            , Svg.Attributes.strokeWidth "2"
            , Svg.Attributes.fill "white"
            ]
    in
    Svg.g
        gAttr
        [ Svg.path
            pathAttr
            []
        ]


viewSnakeCell : Int -> Model.SnakeCell -> Html Msg
viewSnakeCell index snakeCell =
    let
        ( ex, ey ) =
            snakeCell.position

        ( x, y ) =
            ( toFloat (ex * cellSize), toFloat (ey * cellSize) )

        direction =
            case snakeCell.direction of
                Model.Up ->
                    0

                Model.Right ->
                    90

                Model.Down ->
                    180

                Model.Left ->
                    -90

                _ ->
                    0

        gAttr =
            [ Svg.Attributes.transform
                (String.join " "
                    [ interpolate "rotate({0}, {1}, {2})" [ String.fromInt direction, String.fromFloat (x + (cellSizeFloat / 2)), String.fromFloat (y + (cellSizeFloat / 2)) ]
                    , interpolate "translate({0}, {1})" [ String.fromFloat x, String.fromFloat y ]
                    ]
                )
            ]

        circleAttr =
            [ Svg.Attributes.cx (String.fromFloat (cellSizeFloat / 2))
            , Svg.Attributes.cy (String.fromFloat (cellSizeFloat / 2))
            , Svg.Attributes.r (String.fromFloat (cellSizeFloat * 0.36))
            , Svg.Attributes.stroke
                (if index == 0 then
                    "tomato"

                 else
                    "black"
                )
            , Svg.Attributes.strokeWidth
                (if index == 0 then
                    "5"

                 else
                    "2"
                )
            , Svg.Attributes.fill "white"
            ]
    in
    Svg.g
        gAttr
        [ Svg.circle
            circleAttr
            []
        , Svg.text_
            [ Svg.Attributes.x (String.fromFloat (cellSizeFloat * 0.5))
            , Svg.Attributes.y (String.fromFloat (cellSizeFloat * 0.55))
            , Svg.Attributes.fontSize (String.fromFloat (cellSizeFloat * 0.4))
            , Svg.Attributes.dominantBaseline "middle"
            , Svg.Attributes.textAnchor "middle"
            ]
            [ Svg.text
                (if snakeCell.okada == Model.Oka then
                    "岡"

                 else
                    "田"
                )
            ]
        ]


viewButton : Model.Direction -> String -> Html Msg
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


viewScore : Model.Snake -> Html Msg
viewScore player =
    let
        score =
            List.length (Model.snakeCells player) - 1
    in
    div
        [ style "padding" "0.2rem 1rem"
        , style "width" "5rem"
        , style "text-align" "right"
        , style "color" "#fff"
        , style "background-color" "#333"
        ]
        [ text (String.fromInt score) ]



-- SUBSCRIPTIONS


subscriptions : Model.Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ onKeyDown (Decode.map Change keyDecoder)
        ]


keyDecoder : Decode.Decoder Model.Direction
keyDecoder =
    Decode.map toDirection (Decode.field "key" Decode.string)


toDirection : String -> Model.Direction
toDirection string =
    case string of
        "ArrowLeft" ->
            Model.Left

        "ArrowRight" ->
            Model.Right

        "ArrowUp" ->
            Model.Up

        "ArrowDown" ->
            Model.Down

        _ ->
            Model.Other
