module App exposing (..)

import Html.App as App
import Model exposing (init)
import Update exposing (update)
import View exposing (view)
import Subscriptions exposing (subscriptions)
import Random


main : Program Never
main =
    let
        seed =
            Random.initialSeed 67246199
    in
        App.program
            { init = (init seed)
            , update = update
            , view = view
            , subscriptions = subscriptions
            }
