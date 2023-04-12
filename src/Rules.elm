module Rules exposing (..)


type Marker
    = X
    | O


type Player
    = Player Marker


type alias Players =
    ( Player, Player )


type alias Turns =
    Int


type GameState
    = Unstarted
    | InProgress ( Player, Turns )
    | Drawn Turns
    | WonBy ( Player, Turns )


fmtPlayer : Player -> String
fmtPlayer (Player marker) =
    case marker of
        X ->
            "Player X"

        O ->
            "Player O"
