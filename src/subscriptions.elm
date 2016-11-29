module Subscriptions exposing (subscriptions)

import Model
import Keyboard
import Time
import Char


subscriptions : Model.Model -> Sub Model.Msg
subscriptions model =
    Sub.batch
        [ Keyboard.downs (\code -> Model.Downs (Char.fromCode code))
        , Keyboard.ups (\code -> Model.Ups (Char.fromCode code))
        , tickSubscription model
        ]



{--Only track ticks during running rounds
--}


tickSubscription : Model.Model -> Sub Model.Msg
tickSubscription model =
    case model.state of
        Model.Running _ ->
            Time.every (20 * Time.millisecond) Model.Tick

        _ ->
            Sub.none
