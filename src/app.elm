module App exposing (..)

import Model
import Update
import View
import Subscriptions
import Html


{-|
Basic access to program.  No ports or flags required.
Names are rather unimaginative
-}
main : Program Model.Flags Model.Model Model.Msg
main =
    Html.programWithFlags
        { init = Model.init
        , update = Update.update
        , view = View.view
        , subscriptions = Subscriptions.subscriptions
        }
