module Model exposing
    ( Cell(..)
    , Direction(..)
    , Model
    , Okada(..)
    , Position
    , Row
    , Snake
    , SnakeCell
    , cellCount
    , generateBlock
    , movePlayer
    , snakeCells
    )

import Array



-- MODEL


cellCount : Int
cellCount =
    10


cellCountFloat : Float
cellCountFloat =
    toFloat cellCount


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



-- UPDATE


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


movePlayer : Direction -> ( Field, Snake ) -> ( Field, Snake )
movePlayer direction ( field, player ) =
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

        fieldCellMaybe =
            List.head (filterCells (\( p, _ ) -> p == ( nextTopX, nextTopY )) field)
    in
    if List.member ( nextTopX, nextTopY ) (List.map (\snakeCell -> snakeCell.position) cellList) then
        ( field, player )

    else
        fieldCellMaybe
            |> Maybe.map
                (\( _, fieldCell ) ->
                    let
                        eat =
                            (fieldCell == Block Oka && modBy 2 (List.length body) == 1)
                                || (fieldCell == Block Da && modBy 2 (List.length body) == 0)
                    in
                    if fieldCell == Empty || eat then
                        ( List.map
                            (\( p, c ) ->
                                if p == ( nextTopX, nextTopY ) then
                                    ( p, Empty )

                                else
                                    ( p, c )
                            )
                            (allCells field)
                            |> cellsToField
                        , ( { head | position = ( nextTopX, nextTopY ), direction = direction }
                          , List.append
                                (List.indexedMap
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
                                (if eat then
                                    List.head (List.reverse cellList)
                                        |> Maybe.map
                                            (\tail ->
                                                [ { position = tail.position
                                                  , okada =
                                                        if fieldCell == Block Oka then
                                                            Oka

                                                        else
                                                            Da
                                                  , direction = tail.direction
                                                  }
                                                ]
                                            )
                                        |> Maybe.withDefault []

                                 else
                                    []
                                )
                          )
                        )

                    else
                        ( field, player )
                )
            |> Maybe.withDefault
                ( field, player )
