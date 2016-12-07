module Model exposing (..)

import Time
import Random


-- Screen size is fixed for this game and the dimensions are defined here.


screenWidth : Int
screenWidth =
    600


screenHeight : Int
screenHeight =
    600


midX : Int
midX =
    (screenWidth // 2)


midY : Int
midY =
    (screenHeight // 2)


initialHeading : Int
initialHeading =
    270


type alias Score =
    Int


type alias Point =
    { x : Int, y : Int }


type alias Velocity =
    ( Float, Float )


type alias Lives =
    Int


type Rotating
    = Not
    | Clockwise
    | Anticlockwise


type AppState
    = Start
    | Running Round
    | GameOver Int


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
    , rotation : Int
    , angle : Int
    }


type alias Round =
    { tick : Int
    , ship : Ship
    , bullets : List Bullet
    , rocks : List Rock
    , score : Score
    }


type alias Model =
    { state : AppState
    , lives : Lives
    , difficulty : Int
    , seed : Random.Seed
    , highScore : Int
    }


type alias Flags =
    { randSeed : Int
    }



-- INIT


{-|
init creates a model
-}
init : Flags -> ( Model, Cmd Msg )
init { randSeed } =
    ( Model Start
        3
        1
        (Random.initialSeed randSeed)
        0
    , Cmd.none
    )


startingShip : Ship
startingShip =
    { pos = { x = midX, y = midY }
    , velocity = ( 0, degrees (toFloat initialHeading) )
    , heading = initialHeading
    , rotating = Not
    , accelerating = False
    , firing = False
    , bullets = 15
    }


genGame : Int -> Random.Seed -> ( Model, Cmd Msg )
genGame highScore seed =
    let
        ( round, nextSeed ) =
            genRound 1 0 seed
    in
        ( Model (Running round) 3 1 nextSeed highScore, Cmd.none )


genRound : Int -> Score -> Random.Seed -> ( Round, Random.Seed )
genRound difficulty score seed =
    let
        ( rocks, nextSeed ) =
            genRocks difficulty seed
    in
        ( { tick = 0
          , ship = startingShip
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

        ( spin, seed3 ) =
            spinDir seed2
    in
        ( (Rock position velocity 64 spin 0) :: rocks, seed3 )


genRocks : Int -> Random.Seed -> ( List Rock, Random.Seed )
genRocks difficulty seed =
    List.foldl (genRock difficulty) ( [], seed ) (List.range 0 5)


spinDir : Random.Seed -> ( Int, Random.Seed )
spinDir seed =
    Random.step (Random.int -1 1) seed


type Msg
    = Tick Time.Time
    | Action Act
    | NewRound


type Act
    = Accelerate
    | RotateLeft
    | RotateRight
    | Fire
    | Coast
    | MaintainHeading
    | CeaseFire
    | None
