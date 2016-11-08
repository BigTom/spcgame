module Subscriptions exposing (..)

import Model
import Keyboard
import Time
import Char


subscriptions : Model.Model -> Sub Model.Msg
subscriptions _ =
    Sub.batch
        [ Keyboard.downs (\code -> Model.Downs (Char.fromCode code))
        , Keyboard.ups (\code -> Model.Ups (Char.fromCode code))
        , Time.every (20 * Time.millisecond) Model.Tick
        ]
