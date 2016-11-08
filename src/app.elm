module App exposing (..)

import Html.App as App
import Model
import Update
import View
import Subscriptions


main : Program Never
main =
    App.program
        { init = Model.init
        , update = Update.update
        , view = View.view
        , subscriptions = Subscriptions.subscriptions
        }
