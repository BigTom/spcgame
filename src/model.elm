module Model exposing (..)

import Time
import Random
import Window


--- Screen size is fixed for this game and the dimensions are defined here.


stdMaxX : Int
stdMaxX =
    600


stdMaxY : Int
stdMaxY =
    600


stdMidX : Int
stdMidX =
    (stdMaxX // 2)


stdMidY : Int
stdMidY =
    (stdMaxY // 2)


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


type alias Win =
    { maxX : Int
    , maxY : Int
    , midX : Int
    , midY : Int
    }


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
    , win : Win
    }


type alias Flags =
    { randSeed : Int
    , width : Int
    , height : Int
    }



-- INIT


{-|
init creates a model
-}
init : Flags -> ( Model, Cmd Msg )
init { randSeed, width, height } =
    ( Model Start
        3
        1
        (Random.initialSeed randSeed)
        0
        (Win width (height - 100) (width // 2) ((height - 100) // 2))
    , Cmd.none
    )


startingShip : Win -> Ship
startingShip { midX, midY } =
    { pos = { x = midX, y = midY }
    , velocity = ( 0, degrees (toFloat initialHeading) )
    , heading = initialHeading
    , rotating = Not
    , accelerating = False
    , firing = False
    , bullets = 15
    }


genGame : Int -> Random.Seed -> Win -> ( Model, Cmd Msg )
genGame highScore seed win =
    let
        ( round, nextSeed ) =
            genRound win 1 0 seed
    in
        ( Model (Running round) 3 1 nextSeed highScore win, Cmd.none )


genRound : Win -> Int -> Score -> Random.Seed -> ( Round, Random.Seed )
genRound win difficulty score seed =
    let
        ( rocks, nextSeed ) =
            genRocks win difficulty seed
    in
        ( { tick = 0
          , ship = (startingShip win)
          , bullets = []
          , rocks = rocks
          , score = score
          }
        , nextSeed
        )


genRockPos : Win -> Random.Seed -> ( Point, Random.Seed )
genRockPos win seed =
    let
        ( left, ls ) =
            Random.step Random.bool seed

        ( px, xs ) =
            Random.step (Random.int 0 (win.maxX // 3)) ls

        x =
            if left then
                px
            else
                win.maxX - px

        ( top, ts ) =
            Random.step Random.bool xs

        ( py, ys ) =
            Random.step (Random.int 0 (win.maxY // 3)) ts

        y =
            if top then
                py
            else
                win.maxY - py
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


genRock : Win -> Int -> Int -> ( List Rock, Random.Seed ) -> ( List Rock, Random.Seed )
genRock win _ difficulty ( rocks, seed ) =
    let
        ( position, seed1 ) =
            genRockPos win seed

        ( velocity, seed2 ) =
            genVelocity difficulty seed1

        ( spin, seed3 ) =
            spinDir seed2
    in
        ( (Rock position velocity 64 spin 0) :: rocks, seed3 )


genRocks : Win -> Int -> Random.Seed -> ( List Rock, Random.Seed )
genRocks win difficulty seed =
    List.foldl (genRock win difficulty) ( [], seed ) (List.range 0 5)


spinDir : Random.Seed -> ( Int, Random.Seed )
spinDir seed =
    Random.step (Random.int -1 1) seed


type Msg
    = Tick Time.Time
    | Action Act
    | NewRound
    | Resize Window.Size


type Act
    = Accelerate
    | RotateLeft
    | RotateRight
    | Fire
    | Coast
    | MaintainHeading
    | CeaseFire
    | None
