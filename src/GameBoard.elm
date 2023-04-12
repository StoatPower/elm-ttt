module GameBoard exposing (..)

import Color
import Dict exposing (Dict, get)
import Element exposing (..)
import Element.Background as Background
import Element.Events exposing (onClick)
import List.Extra as LEx
import Message exposing (Msg(..))
import Rules exposing (Marker(..), Player(..))


type CellState
    = Empty
    | Marked Marker


type alias CellPosition =
    ( Int, Int )


type alias Board =
    Dict CellPosition CellState


type alias Grid =
    List (List ( CellPosition, CellState ))


initBoard : Board
initBoard =
    let
        range =
            List.range 0 2
    in
    range
        |> List.concatMap
            (\x ->
                List.map (initCell x) range
            )
        |> Dict.fromList


initCell : Int -> Int -> ( CellPosition, CellState )
initCell x y =
    ( ( x, y ), Empty )


updateBoard : CellPosition -> Player -> Board -> Board
updateBoard cellPosition (Player marker) board =
    board
        |> Dict.insert cellPosition (Marked marker)


anyMatchesForPlayer : Player -> Board -> Bool
anyMatchesForPlayer player board =
    [ anyRowMatch player board
    , anyColumnMatch player board
    , anyDiagonalMatch player board
    ]
        |> List.any (\result -> result == True)


anyRowMatch : Player -> Board -> Bool
anyRowMatch (Player marker) board =
    board
        |> Dict.toList
        |> LEx.groupsOf (boardDimension board)
        |> List.any (assessMatch marker)


anyColumnMatch : Player -> Board -> Bool
anyColumnMatch (Player marker) board =
    board
        |> Dict.toList
        |> List.sortBy
            (\( ( _, y ), _ ) -> y)
        |> LEx.groupsOf (boardDimension board)
        |> List.any (assessMatch marker)


anyDiagonalMatch : Player -> Board -> Bool
anyDiagonalMatch (Player marker) board =
    let
        dimension =
            boardDimension board

        maxIndex =
            dimension - 1

        xs =
            List.range 0 maxIndex

        ys =
            List.reverse xs

        getDiagonal : List CellPosition -> List ( CellPosition, CellState )
        getDiagonal keys =
            keys
                |> List.filterMap
                    (\key ->
                        board
                            |> Dict.get key
                            |> Maybe.map (Tuple.pair key)
                    )

        downDiag : List ( CellPosition, CellState )
        downDiag =
            xs
                |> LEx.zip xs
                |> getDiagonal

        upDiag : List ( CellPosition, CellState )
        upDiag =
            xs
                |> LEx.zip ys
                |> getDiagonal
    in
    [ downDiag, upDiag ]
        |> List.any (assessMatch marker)


assessMatch : Marker -> List ( CellPosition, CellState ) -> Bool
assessMatch marker cells =
    cells
        |> List.all
            (\( _, state ) ->
                case state of
                    Marked mark ->
                        mark == marker

                    _ ->
                        False
            )


boardToGrid : Board -> Grid
boardToGrid board =
    board
        |> Dict.toList
        |> LEx.groupsOf (boardDimension board)


boardDimension : Board -> Int
boardDimension board =
    board
        |> Dict.size
        |> toFloat
        |> sqrt
        |> floor


fmtCellState : CellState -> Element msg
fmtCellState state =
    case state of
        Empty ->
            none

        Marked X ->
            text "x"

        Marked O ->
            text "o"


view : Board -> Element Msg
view board =
    board
        |> boardToGrid
        |> gridView { readonly = False }
        |> el []


readonlyView : Board -> Element Msg
readonlyView board =
    board
        |> boardToGrid
        |> gridView { readonly = True }
        |> el []


gridView : { opts | readonly : Bool } -> Grid -> Element Msg
gridView opts grid =
    grid
        |> List.map (rowView opts)
        |> column
            [ spacingXY 0 5
            , Background.color <| fromRgb <| Color.toRgba <| Color.blue
            ]


rowView : { opts | readonly : Bool } -> List ( CellPosition, CellState ) -> Element Msg
rowView opts cells =
    cells
        |> List.map (cellView opts)
        |> row
            [ height <| px 250
            , Background.color <| fromRgb <| Color.toRgba <| Color.blue
            , spacingXY 5 0
            ]


cellView : { opts | readonly : Bool } -> ( CellPosition, CellState ) -> Element Msg
cellView opts ( cellPosition, cellState ) =
    el
        [ width <| px 250
        , height fill
        , Background.color <| fromRgb <| Color.toRgba <| Color.white
        , onClick <|
            case ( opts.readonly == True, cellState ) of
                ( False, Empty ) ->
                    SelectCell cellPosition

                _ ->
                    NoOp
        ]
        (cellContents cellState)


cellContents : CellState -> Element Msg
cellContents state =
    el
        [ centerX
        , centerY
        , Background.color <| fromRgb <| Color.toRgba <| Color.white
        ]
        (fmtCellState state)
