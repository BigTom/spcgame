module Model exposing (..)

import Time
import Random


screenWidth : Int
screenWidth =
    600


screenHeight : Int
screenHeight =
    600


initialHeading : Int
initialHeading =
    270


type alias Score =
    Int


type alias Point =
    { x : Int, y : Int }


type alias Velocity =
    ( Float, Float )


type Rotating
    = Not
    | Clockwise
    | Anticlockwise


type alias Ship =
    { pos : Point
    , velocity : Velocity
    , heading : Int
    , accelerating : Bool
    , rotating : Rotating
    , firing : Bool
    , bullets : Int
    }


type alias Bullet =
    { fired : Int
    , pos : Point
    , velocity : Velocity
    }


type alias Rock =
    { pos : Point
    , velocity : Velocity
    , radius : Int
    }


type alias Game =
    { tick : Int
    , ship : Ship
    , bullets : List Bullet
    , rocks : List Rock
    , score : Score
    }


type alias Model =
    { currentGame : Game
    , lives : Int
    , difficulty : Int
    , seed : Random.Seed
    }



-- init


init : Random.Seed -> ( Model, Cmd Msg )
init seed =
    let
        ( game, nextSeed ) =
            -- genGame 1 (Random.initialSeed 78641289470)
            genGame 1 0 seed
    in
        ( Model game 3 1 nextSeed
        , Cmd.none
        )


genGame : Int -> Score -> Random.Seed -> ( Game, Random.Seed )
genGame difficulty score seed =
    let
        ( rocks, nextSeed ) =
            initRocks difficulty seed
    in
        ( { tick = 0
          , ship =
                { pos = { x = screenWidth // 2, y = screenHeight // 2 }
                , velocity = ( 0, degrees (toFloat initialHeading) )
                , heading = initialHeading
                , rotating = Not
                , accelerating = False
                , firing = False
                , bullets = 6
                }
          , bullets = []
          , rocks = rocks
          , score = score
          }
        , nextSeed
        )


genRockPos : Random.Seed -> ( Point, Random.Seed )
genRockPos seed =
    let
        ( left, ls ) =
            Random.step Random.bool seed

        ( px, xs ) =
            Random.step (Random.int 0 (screenWidth // 3)) ls

        x =
            if left then
                px
            else
                screenWidth - px

        ( top, ts ) =
            Random.step Random.bool xs

        ( py, ys ) =
            Random.step (Random.int 0 (screenHeight // 3)) ts

        y =
            if top then
                py
            else
                screenHeight - py
    in
        ( { x = x, y = y }, ys )


genVelocity : Int -> Random.Seed -> ( Velocity, Random.Seed )
genVelocity difficulty seed =
    let
        ( rad, rs ) =
            Random.step (Random.float 0.6 0.9) seed

        ( theta, ts ) =
            Random.step (Random.float 0 pi) rs

        inc =
            (toFloat (difficulty + 10)) / 10
    in
        ( ( rad * inc, theta ), ts )


genRock : Int -> Int -> ( List Rock, Random.Seed ) -> ( List Rock, Random.Seed )
genRock _ difficulty ( rocks, seed ) =
    let
        ( position, seed1 ) =
            genRockPos seed

        ( velocity, seed2 ) =
            genVelocity difficulty seed1
    in
        ( (Rock position velocity 64) :: rocks, seed2 )


initRocks : Int -> Random.Seed -> ( List Rock, Random.Seed )
initRocks difficulty seed =
    List.foldl (genRock difficulty) ( [], seed ) [0..5]



-- MESSAGES


type Msg
    = Tick Time.Time
    | Downs Char
    | Ups Char
    | WonGame
    | LostGame
