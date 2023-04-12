module Message exposing (..)


type Msg
    = NoOp
    | StartGame
    | SelectCell ( Int, Int )
