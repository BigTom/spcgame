module App exposing (..)

import Model
import Update
import View
import Subscriptions
import Html


main : Program Never Model.Model Model.Msg
main =
    Html.program
        { init = Model.init
        , update = Update.update
        , view = View.view
        , subscriptions = Subscriptions.subscriptions
        }
