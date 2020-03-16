module Main exposing (main)

import Array exposing (Array)
import Browser
import Browser.Events exposing (onKeyDown)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Json.Decode as Decode
import Random
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
    , steps : List Direction
    , player : Snake
    }


type alias Field =
    List Row


type alias Row =
    List Cell


type Cell
    = Block Okada
    | Empty


type alias CellWithPosition =
    ( Position, Cell )


type Okada
    = Oka
    | Da


type alias SnakeCell =
    { position : Position
    , okada : Okada
    , direction : Direction
    }


type alias Snake =
    ( SnakeCell, List SnakeCell )


snakeCells : Snake -> List SnakeCell
snakeCells snake =
    let
        ( head, body ) =
            snake
    in
    List.append [ head ] body


allCells : Field -> List CellWithPosition
allCells field =
    List.indexedMap
        (\y row ->
            List.indexedMap
                (\x cell ->
                    ( ( x, y ), cell )
                )
                row
        )
        field
        |> List.concat


cellsToField : List CellWithPosition -> Field
cellsToField list =
    List.map
        (\axisY ->
            List.partition (\( ( _, y ), _ ) -> y == axisY) list
                |> (\( group, _ ) ->
                        List.sortBy (\( ( x, _ ), _ ) -> x) group
                   )
                |> List.map (\( _, c ) -> c)
        )
        (List.range 0 (cellCount - 1))


filterCells : (CellWithPosition -> Bool) -> Field -> List CellWithPosition
filterCells valid field =
    allCells field
        |> List.filter valid


emptyCells : ( Field, Snake ) -> List CellWithPosition
emptyCells ( field, player ) =
    filterCells
        (\( position, cell ) ->
            cell
                == Empty
                && List.member position
                    (snakeCells player
                        |> List.map (\sc -> sc.position)
                    )
                == False
        )
        field


type alias Position =
    ( Int, Int )


type Direction
    = Left
    | Right
    | Up
    | Down
    | Other


init : () -> ( Model, Cmd Msg )
init _ =
    ( { field =
            List.repeat 10 <| List.repeat 10 <| Empty
      , player =
            ( { position = ( 0, cellCount - 4 ), okada = Oka, direction = Up }
            , [ { position = ( 0, cellCount - 3 ), okada = Da, direction = Up }
              , { position = ( 0, cellCount - 2 ), okada = Oka, direction = Up }
              , { position = ( 0, cellCount - 1 ), okada = Da, direction = Up }
              ]
            )
      , steps = []
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = Change Direction
    | GenerateBlock Position


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Change direction ->
            if direction == Other then
                ( model, Cmd.none )

            else
                ( { model
                    | player = movePlayer direction model.player
                    , steps = List.append model.steps [ direction ]
                  }
                , Random.generate GenerateBlock (Random.pair (Random.int 0 (cellCount - 1)) (Random.int 0 (cellCount - 1)))
                )

        GenerateBlock pos ->
            ( { model
                | field =
                    generateBlock ( model.field, model.player )
                        pos
                        (if modBy 2 (List.length model.steps) == 1 then
                            Oka

                         else
                            Da
                        )
              }
            , Cmd.none
            )


generateBlock : ( Field, Snake ) -> Position -> Okada -> Field
generateBlock ( field, player ) p okada =
    let
        empties =
            emptyCells ( field, player )

        emptyPositions =
            List.map (\( pos, _ ) -> pos) empties

        notEmpty =
            filterCells (\( pos, cell ) -> List.member pos emptyPositions == False) field
    in
    List.append notEmpty
        (empties
            |> List.map
                (\( position, cell ) ->
                    if position == p then
                        ( position, Block okada )

                    else
                        ( position, cell )
                )
        )
        |> cellsToField


movePlayer : Direction -> Snake -> Snake
movePlayer direction player =
    let
        ( dx, dy ) =
            case direction of
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

        ( head, body ) =
            player

        cellList =
            snakeCells player

        ( headX, headY ) =
            head.position

        nextTopX =
            Basics.min (cellCount - 1) (Basics.max 0 (headX + dx))

        nextTopY =
            Basics.min (cellCount - 1) (Basics.max 0 (headY + dy))
    in
    if List.member ( nextTopX, nextTopY ) (List.map (\snakeCell -> snakeCell.position) cellList) then
        player

    else
        ( { head | position = ( nextTopX, nextTopY ), direction = direction }
        , List.indexedMap
            (\i snakeCell ->
                let
                    mayBeBefore =
                        Array.get i (Array.fromList cellList)
                in
                mayBeBefore
                    |> Maybe.map
                        (\before ->
                            { snakeCell
                                | position = before.position
                                , direction = before.direction
                            }
                        )
                    |> Maybe.withDefault snakeCell
            )
            body
        )



-- VIEW


cellCount : Int
cellCount =
    10


cellCountFloat : Float
cellCountFloat =
    toFloat cellCount


cellSize : Int
cellSize =
    40


cellSizeFloat : Float
cellSizeFloat =
    toFloat cellSize


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
        List.indexedMap (\y row -> viewRow row y) model.field
            ++ [ viewPlayer model.player ]


viewRow : Row -> Int -> Html Msg
viewRow row y =
    Svg.g [] <|
        List.indexedMap (\x cell -> viewCell cell ( x, y )) row


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
                    [ Svg.Attributes.x (String.fromFloat (cellSizeFloat * 0.5))
                    , Svg.Attributes.y (String.fromFloat (cellSizeFloat * 0.55))
                    , Svg.Attributes.fontSize (String.fromFloat (cellSizeFloat * 0.6))
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
    let
        ( head, body ) =
            player

        cells =
            snakeCells player
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


viewSnakeLine : SnakeCell -> SnakeCell -> Html Msg
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


viewSnakeCell : Int -> SnakeCell -> Html Msg
viewSnakeCell index snakeCell =
    let
        ( ex, ey ) =
            snakeCell.position

        ( x, y ) =
            ( toFloat (ex * cellSize), toFloat (ey * cellSize) )

        direction =
            case snakeCell.direction of
                Up ->
                    0

                Right ->
                    90

                Down ->
                    180

                Left ->
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
                (if snakeCell.okada == Oka then
                    "岡"

                 else
                    "田"
                )
            ]
        ]


viewButton : Direction -> String -> Html Msg
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
subscriptions _ =
    Sub.batch
        [ onKeyDown (Decode.map Change keyDecoder)
        ]


keyDecoder : Decode.Decoder Direction
keyDecoder =
    Decode.map toDirection (Decode.field "key" Decode.string)


toDirection : String -> Direction
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
