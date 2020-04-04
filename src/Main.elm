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
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- UPDATE


initialCount : Int
initialCount =
    5


score : Model.Snake -> Int
score player =
    List.length (Model.snakeCells player) - 1


nextExpandAfter : ( Model.Field, Model.Snake ) -> Int
nextExpandAfter ( field, player ) =
    List.sum (List.range (initialCount - 1) (Model.cellCount field - 1)) - score player


init : () -> ( Model.Model, Cmd Msg )
init _ =
    ( Model.reset initialCount
    , Cmd.none
    )


type Msg
    = Change Model.Direction
    | Reset
    | GenerateBlock Model.Position


update : Msg -> Model.Model -> ( Model.Model, Cmd Msg )
update msg model =
    case msg of
        Change direction ->
            if direction == Model.Other then
                ( model, Cmd.none )

            else
                let
                    count =
                        Model.cellCount model.field

                    ( field, player ) =
                        Model.movePlayer direction ( model.field, model.player )

                    ( beforeHead, _ ) =
                        model.player

                    ( head, _ ) =
                        player

                    isMoved =
                        beforeHead.position /= head.position

                    isExpanded =
                        nextExpandAfter ( field, player ) == 0

                    ( nextField, nextPlayer ) =
                        if isExpanded then
                            Model.expandField ( field, player )

                        else
                            ( field, player )
                in
                if isMoved then
                    ( { model
                        | player = nextPlayer
                        , field = nextField
                        , steps = List.append model.steps [ direction ]
                      }
                    , Random.generate GenerateBlock (Random.pair (Random.int 0 (count - 1)) (Random.int 0 (count - 1)))
                    )

                else
                    ( model, Cmd.none )

        Reset ->
            ( Model.reset initialCount, Cmd.none )

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
                            (if modBy 4 (List.length model.steps) == 0 then
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


view : Model.Model -> Browser.Document Msg
view model =
    let
        isGameOver =
            Model.isGameOver model
    in
    { title = "Okada Snake"
    , body =
        [ div
            [ style "padding" "1rem"
            , style "display" "flex"
            , style "flex-direction" "column"
            , style "align-items" "center"
            ]
            [ div
                [ style "position" "relative"
                , style "display" "flex"
                ]
                (List.append
                    [ viewField model, nextExpandAfterView ( model.field, model.player ) ]
                    (if isGameOver then
                        [ viewGameOver ]

                     else
                        []
                    )
                )
            , div
                [ style "margin" "0.5rem"
                , style "display" "flex"
                , style "align-items" "flex-start"
                , style "justify-content" "space-between"
                ]
                [ div
                    [ style "margin" "0 2rem"
                    , style "display" "flex"
                    , style "flex-direction" "column"
                    ]
                    [ viewScore model
                    , viewResetButton
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
            , div []
                [ a
                    [ href "https://github.com/miyanokomiya/okada-snake"
                    , Html.Attributes.target "_blank"
                    , Html.Attributes.rel "noopener"
                    , style "font-size" "0.8rem"
                    ]
                    [ text "repository" ]
                ]
            ]
        ]
    }


viewField : Model.Model -> Html Msg
viewField model =
    let
        count =
            Model.cellCount model.field

        enablePositions =
            Model.enablePositions ( model.field, model.player )
    in
    Svg.svg
        [ Svg.Attributes.width "400"
        , Svg.Attributes.height "400"
        , Svg.Attributes.viewBox (interpolate "0 0 {0} {1}" [ String.fromInt (count * cellSize), String.fromInt (count * cellSize) ])
        , style "border" "1px solid black"
        ]
    <|
        (Model.allCells model.field
            |> List.map (\( p, c ) -> viewCell c p (List.member p enablePositions))
        )
            ++ [ viewPlayer model.player ]


viewCell : Model.Cell -> Model.Position -> Bool -> Html Msg
viewCell cell ( x, y ) enable =
    let
        gAttr =
            [ Svg.Attributes.transform (interpolate "translate({0}, {1})" [ String.fromInt (x * cellSize), String.fromInt (y * cellSize) ])
            ]

        rectAttr =
            [ Svg.Attributes.width (String.fromInt cellSize)
            , Svg.Attributes.height (String.fromInt cellSize)
            , Svg.Attributes.stroke "black"
            , Svg.Attributes.fill
                (if enable then
                    "lime"

                 else
                    "none"
                )
            ]
    in
    case cell of
        Model.Block okada count ->
            Svg.g
                gAttr
                [ Svg.rect
                    rectAttr
                    []
                , Svg.text_
                    [ Svg.Attributes.x (String.fromFloat (cellSizeFloat * 0.5))
                    , Svg.Attributes.y (String.fromFloat (cellSizeFloat * 0.55))
                    , Svg.Attributes.fontSize (String.fromFloat (cellSizeFloat * 0.6 / toFloat (1 + count)))
                    , Svg.Attributes.dominantBaseline "middle"
                    , Svg.Attributes.textAnchor "middle"
                    , Svg.Attributes.fill
                        (case count of
                            1 ->
                                "blue"

                            2 ->
                                "red"

                            _ ->
                                "black"
                        )
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


viewResetButton : Html Msg
viewResetButton =
    let
        buttonStyle =
            [ style "margin-top" "0.5rem"
            , style "width" "4rem"
            , style "height" "1.5rem"
            , style "border" "1px solid black"
            ]
    in
    button (List.append buttonStyle [ onClick Reset ]) [ text "Reset" ]


viewScore : Model.Model -> Html Msg
viewScore model =
    div
        [ style "padding" "0.2rem 1rem"
        , style "width" "5rem"
        , style "text-align" "right"
        , style "color" "#fff"
        , style "background-color" "#333"
        ]
        [ text (String.fromInt (score model.player)) ]


viewGameOver : Html Msg
viewGameOver =
    div
        [ style "position" "absolute"
        , style "top" "50%"
        , style "left" "50%"
        , style "transform" "translate(-50%, -50%)"
        , style "font-size" "3rem"
        , style "font-weight" "600"
        , style "color" "red"
        , style "white-space" "nowrap"
        ]
        [ text "Game Over" ]



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


nextExpandAfterView : ( Model.Field, Model.Snake ) -> Html Msg
nextExpandAfterView ( field, player ) =
    div
        [ style "position" "absolute"
        , style "bottom" "0"
        , style "right" "0"
        , style "padding" "0 0.5rem"
        , style "display" "flex"
        , style "align-items" "center"
        , style "justify-content" "center"
        , style "transform" "translate(50%, 50%)"
        , style "font-size" "1.2rem"
        , style "background-color" "white"
        , style "border" "4px solid lime"
        , style "border-radius" "50%"
        ]
        [ text (String.fromInt (nextExpandAfter ( field, player ))) ]
