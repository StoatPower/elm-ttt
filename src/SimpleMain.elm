module SimpleMain exposing (..)

import Array exposing (Array)
import Browser exposing (Document)
import Color
import Element as El exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font as Font
import Element.Input as Input
import GameBoard exposing (anyDiagonalMatch)
import Html exposing (sub)


main : Program () Model Msg
main =
    Browser.document
        { init = \_ -> init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Turns =
    Int


type Player
    = X
    | O


type GameState
    = Unstarted
    | InProgress ( Player, Turns )
    | Drawn Turns
    | WonBy ( Player, Turns )


type alias Model =
    { row0 : Array CellState
    , row1 : Array CellState
    , row2 : Array CellState
    , state : GameState
    }


type CellState
    = Empty
    | Marked Player


type alias CellPosition =
    ( Int, Int )


dimensions : Int
dimensions =
    3


maxTurns : Int
maxTurns =
    9


initRow : Array CellState
initRow =
    Array.repeat dimensions Empty


defaultModel : Model
defaultModel =
    { row0 = initRow
    , row1 = initRow
    , row2 = initRow
    , state = Unstarted
    }


init : ( Model, Cmd Msg )
init =
    ( defaultModel
    , Cmd.none
    )


type Msg
    = NoOp
    | StartGame
    | SelectCell CellPosition


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        StartGame ->
            ( { defaultModel
                | state = InProgress ( X, 1 )
              }
            , Cmd.none
            )

        SelectCell ( x, y ) ->
            case model.state of
                InProgress ( player, turns ) ->
                    let
                        newModel =
                            model
                                |> makeSelection x y player
                                |> nextTurn player turns
                    in
                    ( newModel, Cmd.none )

                _ ->
                    ( model, Cmd.none )


makeSelection : Int -> Int -> Player -> Model -> Model
makeSelection rowIdx colIdx player model =
    let
        updater row =
            row |> Array.set colIdx (Marked player)
    in
    case rowIdx of
        0 ->
            { model | row0 = updater model.row0 }

        1 ->
            { model | row1 = updater model.row1 }

        2 ->
            { model | row2 = updater model.row2 }

        _ ->
            model


nextTurn : Player -> Turns -> Model -> Model
nextTurn player turns model =
    if anyMatchesForPlayer player model == True then
        { model | state = WonBy ( player, turns ) }

    else if turns >= maxTurns then
        { model | state = Drawn turns }

    else
        { model | state = InProgress ( nextPlayer player, turns + 1 ) }


nextPlayer : Player -> Player
nextPlayer player =
    case player of
        X ->
            O

        O ->
            X


anyMatchesForPlayer : Player -> Model -> Bool
anyMatchesForPlayer player { row0, row1, row2 } =
    [ anyRowMatch row0 row1 row2 player
    , anyColumnMatch row0 row1 row2 player
    , anyDiagonalMatch row0 row1 row2 player
    ]
        |> List.any (\result -> result == True)


anyRowMatch : Array CellState -> Array CellState -> Array CellState -> Player -> Bool
anyRowMatch row0 row1 row2 player =
    [ row0 |> Array.toList
    , row1 |> Array.toList
    , row2 |> Array.toList
    ]
        |> List.any (assessMatch player)


anyColumnMatch : Array CellState -> Array CellState -> Array CellState -> Player -> Bool
anyColumnMatch row0 row1 row2 player =
    List.range 0 (dimensions - 1)
        |> List.map
            (\idx ->
                [ getCellState idx row0
                , getCellState idx row1
                , getCellState idx row2
                ]
            )
        |> List.any (assessMatch player)


anyDiagonalMatch : Array CellState -> Array CellState -> Array CellState -> Player -> Bool
anyDiagonalMatch row0 row1 row2 player =
    let
        downDiag =
            [ getCellState 0 row0
            , getCellState 1 row1
            , getCellState 2 row2
            ]

        upDiag =
            [ getCellState 2 row0
            , getCellState 1 row1
            , getCellState 0 row2
            ]
    in
    [ downDiag, upDiag ]
        |> List.any (assessMatch player)


assessMatch : Player -> List CellState -> Bool
assessMatch player cells =
    cells
        |> List.all
            (\state ->
                case state of
                    Marked mark ->
                        mark == player

                    _ ->
                        False
            )


getCellState : Int -> Array CellState -> CellState
getCellState idx row =
    Array.get idx row |> Maybe.withDefault Empty


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Document Msg
view model =
    { title = "Tic Tac Toe"
    , body =
        [ El.layout [ height fill, width fill ] <|
            case model.state of
                Unstarted ->
                    Input.button [ centerX, centerY ]
                        { onPress = Just StartGame
                        , label = text "Start Game"
                        }

                InProgress ( player, turns ) ->
                    column [ centerX, centerY ]
                        [ row []
                            [ el [] <|
                                text <|
                                    "Turn "
                                        ++ String.fromInt turns
                                        ++ ": "
                                        ++ fmtPlayer player
                            ]
                        , boardView model
                        ]

                Drawn turns ->
                    column [ centerX, centerY ]
                        [ row []
                            [ el [] <|
                                text <|
                                    "Turn "
                                        ++ String.fromInt turns
                                        ++ ": "
                                        ++ "Game Drawn. Lame."
                            ]
                        , readonlyBoardView model
                        , Input.button []
                            { onPress = Just StartGame
                            , label = text "Reset Game"
                            }
                        ]

                WonBy ( player, turns ) ->
                    column [ centerX, centerY ]
                        [ row []
                            [ el [] <|
                                text <|
                                    "Turn "
                                        ++ String.fromInt turns
                                        ++ ": "
                                        ++ "Game Won by "
                                        ++ fmtPlayer player
                                        ++ "!"
                            ]
                        , readonlyBoardView model
                        , Input.button []
                            { onPress = Just StartGame
                            , label = text "Reset Game"
                            }
                        ]
        ]
    }


boardView : Model -> Element Msg
boardView model =
    el [] <| gridView { readonly = False } model


readonlyBoardView : Model -> Element Msg
readonlyBoardView model =
    el [] <| gridView { readonly = True } model


gridView : { opts | readonly : Bool } -> Model -> Element Msg
gridView opts { row0, row1, row2 } =
    column
        [ spacingXY 0 5
        , Background.color <| fromRgb <| Color.toRgba <| Color.blue
        ]
        [ rowView opts 0 row0
        , rowView opts 1 row1
        , rowView opts 2 row2
        ]


rowView : { opts | readonly : Bool } -> Int -> Array CellState -> Element Msg
rowView opts rowIdx cells =
    cells
        |> Array.toIndexedList
        |> List.map
            (\( colIdx, state ) ->
                cellView opts ( ( rowIdx, colIdx ), state )
            )
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
            case ( opts.readonly, cellState ) of
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


fmtCellState : CellState -> Element Msg
fmtCellState state =
    case state of
        Empty ->
            none

        Marked X ->
            text "X"

        Marked O ->
            text "O"


fmtPlayer : Player -> String
fmtPlayer player =
    case player of
        X ->
            "Player X"

        O ->
            "Player O"
