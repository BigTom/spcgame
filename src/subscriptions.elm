module Subscriptions exposing (subscriptions)

import Model
import Keyboard
import Time
import Char


subscriptions : Model.Model -> Sub Model.Msg
subscriptions model =
    Sub.batch
        [ Keyboard.downs (downs model)
        , Keyboard.ups (ups model)
        , tickSubscription model
        ]


downs : Model.Model -> Int -> Model.Msg
downs model keyCode =
    let
        char =
            Char.fromCode keyCode
    in
        case char of
            'W' ->
                Model.Action Model.Accelerate

            'D' ->
                Model.Action Model.RotateLeft

            'A' ->
                Model.Action Model.RotateRight

            ' ' ->
                Model.Action Model.Fire

            'B' ->
                case model.state of
                    Model.Running _ ->
                        Model.Action Model.None

                    _ ->
                        Model.NewRound

            _ ->
                Model.Action Model.None


ups : Model.Model -> Int -> Model.Msg
ups model keyCode =
    let
        char =
            Char.fromCode keyCode
    in
        case char of
            'W' ->
                Model.Action Model.Coast

            'D' ->
                Model.Action Model.MaintainHeading

            'A' ->
                Model.Action Model.MaintainHeading

            ' ' ->
                Model.Action Model.CeaseFire

            _ ->
                Model.Action Model.None



{--Only track ticks during running rounds
--}


tickSubscription : Model.Model -> Sub Model.Msg
tickSubscription model =
    case model.state of
        Model.Running _ ->
            Time.every (20 * Time.millisecond) Model.Tick

        _ ->
            Sub.none
