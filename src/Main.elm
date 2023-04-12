module Main exposing (..)

import Browser exposing (Document)
import Element as El exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import GameBoard exposing (..)
import Html exposing (sub)
import Message exposing (Msg(..))
import Rules exposing (..)


type alias Model =
    { board : Board
    , state : GameState
    }


maxTurns : Int
maxTurns =
    9


main : Program () Model Msg
main =
    Browser.document
        { init = \_ -> init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


player1 : Player
player1 =
    Player X


player2 : Player
player2 =
    Player O


players : Players
players =
    ( player1, player2 )


firstPlayer : Players -> Player
firstPlayer ( player, _ ) =
    player


secondPlayer : Players -> Player
secondPlayer ( _, player ) =
    player


init : ( Model, Cmd Msg )
init =
    ( { board = GameBoard.initBoard
      , state = Unstarted
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        StartGame ->
            ( { model
                | board = GameBoard.initBoard
                , state = InProgress ( firstPlayer players, 1 )
              }
            , Cmd.none
            )

        SelectCell cellPosition ->
            case model.state of
                InProgress ( player, turns ) ->
                    let
                        newBoard =
                            model.board
                                |> GameBoard.updateBoard cellPosition player

                        newGameState =
                            nextTurn player turns newBoard
                    in
                    ( { model
                        | board = newBoard
                        , state = newGameState
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )


nextPlayer : Player -> Player
nextPlayer (Player marker) =
    case marker of
        X ->
            secondPlayer players

        O ->
            firstPlayer players


nextTurn : Player -> Turns -> Board -> GameState
nextTurn player turns board =
    if GameBoard.anyMatchesForPlayer player board == True then
        WonBy ( player, turns )

    else if turns >= maxTurns then
        Drawn turns

    else
        InProgress ( nextPlayer player, turns + 1 )


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
                        , GameBoard.view model.board
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
                        , GameBoard.readonlyView model.board
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
                        , GameBoard.readonlyView model.board
                        , Input.button []
                            { onPress = Just StartGame
                            , label = text "Reset Game"
                            }
                        ]
        ]
    }
