module Model exposing
    ( Cell(..)
    , Direction(..)
    , Field
    , Model
    , Okada(..)
    , Position
    , Row
    , Snake
    , SnakeCell
    , allCells
    , cellCount
    , enablePositions
    , expandField
    , generateBlock
    , isGameOver
    , movePlayer
    , reset
    , snakeCells
    )

import Array



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
    = Block Okada Int
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


cellCount : Field -> Int
cellCount field =
    List.length field


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
    let
        count =
            list
                |> List.filter (\( ( x, _ ), _ ) -> x == 0)
                |> List.length
    in
    List.map
        (\axisY ->
            List.partition (\( ( _, y ), _ ) -> y == axisY) list
                |> (\( group, _ ) ->
                        List.sortBy (\( ( x, _ ), _ ) -> x) group
                   )
                |> List.map (\( _, c ) -> c)
        )
        (List.range 0 (count - 1))


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


reset : Int -> Model
reset initialCount =
    { field =
        List.repeat initialCount <| List.repeat initialCount <| Empty
    , player =
        ( { position = ( 0, initialCount - 1 ), okada = Oka, direction = Up }
        , []
        )
    , steps = []
    }


expandField : ( Field, Snake ) -> ( Field, Snake )
expandField ( field, player ) =
    ( allCells field
        |> expandFieldMinus
        |> shiftFieldXY
        |> expandFieldPlus
        |> cellsToField
    , shiftSnake player
    )


shiftSnake : Snake -> Snake
shiftSnake ( head, body ) =
    ( shiftSnakeCell head
    , List.map shiftSnakeCell body
    )


shiftSnakeCell : SnakeCell -> SnakeCell
shiftSnakeCell sc =
    let
        ( x, y ) =
            sc.position
    in
    { sc | position = ( x + 1, y + 1 ) }


shiftFieldXY : List CellWithPosition -> List CellWithPosition
shiftFieldXY list =
    list |> List.map (\( ( x, y ), c ) -> ( ( x + 1, y + 1 ), c ))


expandFieldMinus : List CellWithPosition -> List CellWithPosition
expandFieldMinus list =
    let
        count =
            list
                |> List.filter (\( ( x, _ ), _ ) -> x == 0)
                |> List.length
    in
    list
        |> List.append
            (List.range 0 (count - 1)
                |> List.map (\x -> ( ( x, -1 ), Empty ))
            )
        |> List.append
            (List.range -1 (count - 1)
                |> List.map (\y -> ( ( -1, y ), Empty ))
            )


expandFieldPlus : List CellWithPosition -> List CellWithPosition
expandFieldPlus list =
    let
        count =
            list
                |> List.filter (\( ( x, _ ), _ ) -> x == 0)
                |> List.length
    in
    list
        |> List.append
            (List.range 0 (count - 1)
                |> List.map (\x -> ( ( x, count ), Empty ))
            )
        |> List.append
            (List.range 0 count
                |> List.map (\y -> ( ( count, y ), Empty ))
            )


generateBlock : ( Field, Snake ) -> Position -> Okada -> Field
generateBlock ( field, player ) p okada =
    let
        empties =
            emptyCells ( field, player )

        emptyPositions =
            List.map (\( pos, _ ) -> pos) empties

        notEmpty =
            filterCells (\( pos, _ ) -> List.member pos emptyPositions == False) field

        playerPositions =
            List.map (\s -> s.position) (snakeCells player)
    in
    List.append
        (List.map
            (\( pos, cell ) ->
                if List.member pos playerPositions then
                    ( pos, cell )

                else
                    ( pos, stepBlock cell )
            )
            notEmpty
        )
        (empties
            |> List.map
                (\( position, cell ) ->
                    if position == p then
                        ( position, Block okada 2 )

                    else
                        ( position, cell )
                )
        )
        |> cellsToField


movePlayer : Direction -> ( Field, Snake ) -> ( Field, Snake )
movePlayer direction ( field, player ) =
    let
        ( dx, dy ) =
            directionToDiff direction

        ( head, body ) =
            player

        cellList =
            snakeCells player

        ( headX, headY ) =
            head.position

        nextTopX =
            Basics.min (cellCount field - 1) (Basics.max 0 (headX + dx))

        nextTopY =
            Basics.min (cellCount field - 1) (Basics.max 0 (headY + dy))

        fieldCellMaybe =
            List.head (filterCells (\( p, _ ) -> p == ( nextTopX, nextTopY )) field)
    in
    if List.member ( nextTopX, nextTopY ) (List.map (\snakeCell -> snakeCell.position) cellList) then
        ( field, player )

    else
        fieldCellMaybe
            |> Maybe.map
                (\( fieldCellPos, fieldCell ) ->
                    let
                        canEat =
                            canEatCell player fieldCell

                        canMove =
                            canMoveCell player ( fieldCellPos, fieldCell )
                    in
                    if canMove || canEat then
                        ( List.map
                            (\( p, c ) ->
                                if p == ( nextTopX, nextTopY ) then
                                    ( p
                                    , if canEat then
                                        Empty

                                      else
                                        fieldCell
                                    )

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
                                (if canEat then
                                    List.head (List.reverse cellList)
                                        |> Maybe.map
                                            (\tail ->
                                                [ { position = tail.position
                                                  , okada =
                                                        if fieldCell == Block Oka 0 then
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


enablePositions : ( Field, Snake ) -> List Position
enablePositions ( field, player ) =
    enableDirections ( field, player )
        |> List.map directionToDiff
        |> (let
                ( head, _ ) =
                    player

                ( x, y ) =
                    head.position
            in
            List.map (\( dx, dy ) -> ( x + dx, y + dy ))
           )


isGameOver : Model -> Bool
isGameOver model =
    List.length
        (enablePositions
            ( model.field, model.player )
        )
        == 0



-- private


directionToDiff : Direction -> Position
directionToDiff direction =
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


enableDirections : ( Field, Snake ) -> List Direction
enableDirections ( field, player ) =
    let
        ( head, _ ) =
            player

        ( x, y ) =
            head.position
    in
    [ ( Up, ( x, y - 1 ) )
    , ( Down, ( x, y + 1 ) )
    , ( Left, ( x - 1, y ) )
    , ( Right, ( x + 1, y ) )
    ]
        |> List.filter
            (\( _, dp ) ->
                allCells field
                    |> List.any
                        (\( p, c ) ->
                            p == dp && canMoveCell player ( p, c )
                        )
            )
        |> List.map (\( d, _ ) -> d)


canMoveCell : Snake -> ( Position, Cell ) -> Bool
canMoveCell player ( p, cell ) =
    let
        ( _, body ) =
            player
    in
    List.member p (List.map (\c -> c.position) body) == False && (isCompleteBlock cell == False || canEatCell player cell)


isCompleteBlock : Cell -> Bool
isCompleteBlock cell =
    cell == Block Oka 0 || cell == Block Da 0


canEatCell : Snake -> Cell -> Bool
canEatCell player cell =
    let
        ( _, body ) =
            player
    in
    if modBy 2 (List.length body) == 1 then
        cell == Block Oka 0

    else
        cell == Block Da 0


stepBlock : Cell -> Cell
stepBlock cell =
    case cell of
        Block Oka count ->
            Block Oka (max 0 (count - 1))

        Block Da count ->
            Block Da (max 0 (count - 1))

        Empty ->
            Empty
