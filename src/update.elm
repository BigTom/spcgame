module Update exposing (..)

import Model
import Task


update : Model.Msg -> Model.Model -> ( Model.Model, Cmd Model.Msg )
update msg model =
    if model.lives > 0 then
        case msg of
            Model.LostGame ->
                ( lostGame model, Cmd.none )

            Model.WonGame ->
                ( wonGame model, Cmd.none )

            Model.Tick _ ->
                let
                    ( game, cmd ) =
                        tick model.currentGame
                in
                    ( { model | currentGame = game }, cmd )

            Model.Downs charCode ->
                let
                    game =
                        model.currentGame

                    newGame =
                        { game | ship = shipDowns game.ship charCode }
                in
                    ( { model | currentGame = newGame }, Cmd.none )

            Model.Ups charCode ->
                let
                    game =
                        model.currentGame

                    newGame =
                        { game | ship = shipUps game.ship charCode }
                in
                    ( { model | currentGame = newGame }, Cmd.none )
    else
        ( model, Cmd.none )


lostGame : Model.Model -> Model.Model
lostGame model =
    let
        difficulty =
            1

        ( game, nextSeed ) =
            Model.genGame difficulty model.currentGame.score model.seed
    in
        { model
            | currentGame = game
            , lives = model.lives - 1
            , difficulty = difficulty
            , seed = nextSeed
        }


wonGame : Model.Model -> Model.Model
wonGame model =
    let
        difficulty =
            model.difficulty + 1

        ( game, nextSeed ) =
            Model.genGame difficulty model.currentGame.score model.seed
    in
        { model
            | currentGame = game
            , difficulty = difficulty
            , seed = nextSeed
        }


tick : Model.Game -> ( Model.Game, Cmd Model.Msg )
tick game =
    let
        ( newBullets, remainingBullets ) =
            updateBullets game

        ( newNewBullets, newRocks, score ) =
            bulletHits newBullets (updateRocks game.rocks)

        newGame =
            { game
                | tick = game.tick + 1
                , ship = updateShip game.ship remainingBullets
                , bullets = newNewBullets
                , rocks = newRocks
                , score = game.score + score
            }

        cmd =
            if shipImpact newGame.ship newGame.rocks then
                message Model.LostGame
            else if List.isEmpty newRocks then
                message Model.WonGame
            else
                Cmd.none
    in
        ( newGame
        , cmd
        )


shipDowns : Model.Ship -> Char -> Model.Ship
shipDowns ship charCode =
    case charCode of
        'W' ->
            { ship | accelerating = True }

        'D' ->
            { ship | rotating = Model.Clockwise }

        'A' ->
            { ship | rotating = Model.Anticlockwise }

        ' ' ->
            { ship | firing = True }

        _ ->
            ship


shipUps : Model.Ship -> Char -> Model.Ship
shipUps ship charCode =
    case charCode of
        'W' ->
            { ship | accelerating = False }

        'D' ->
            { ship | rotating = Model.Not }

        'A' ->
            { ship | rotating = Model.Not }

        ' ' ->
            { ship | firing = False }

        _ ->
            ship


v2p : Model.Velocity -> Model.Point
v2p velocity =
    (\( x, y ) -> { x = round x, y = round y })
        (fromPolar velocity)


screenWrap : Model.Point -> Model.Point
screenWrap coord =
    { x = coord.x % Model.screenWidth, y = coord.y % Model.screenHeight }


updateShip : Model.Ship -> Int -> Model.Ship
updateShip ship remainingBullets =
    let
        newVelocity =
            if ship.accelerating then
                accelerateShip ship 1.0
            else
                accelerateShip ship -0.0

        newHeading =
            case ship.rotating of
                Model.Clockwise ->
                    (ship.heading + 3) % 360

                Model.Anticlockwise ->
                    (ship.heading - 3) % 360

                Model.Not ->
                    ship.heading
    in
        { ship
            | pos = move ship.pos ship.velocity
            , velocity = newVelocity
            , heading = newHeading
            , bullets = remainingBullets
        }


accelerateShip : Model.Ship -> Float -> Model.Velocity
accelerateShip { velocity, heading } acc =
    let
        ( vx, vy ) =
            fromPolar velocity

        ( dx, dy ) =
            fromPolar ( acc, degrees (toFloat heading) )
    in
        toPolar ( vx + dx, vy + dy )


updateRocks : List Model.Rock -> List Model.Rock
updateRocks rocks =
    List.map updateRock rocks


move : Model.Point -> Model.Velocity -> Model.Point
move { x, y } v =
    let
        d =
            v2p v
    in
        { x = (x + d.x), y = (y + d.y) } |> screenWrap


updateRock : Model.Rock -> Model.Rock
updateRock rock =
    { rock | pos = move rock.pos rock.velocity }


updateBullets : Model.Game -> ( List Model.Bullet, Int )
updateBullets game =
    let
        ( newBullets, remaingBullets ) =
            if game.ship.firing then
                if game.ship.bullets > 0 then
                    ( fireBullets game, game.ship.bullets - 1 )
                else
                    ( game.bullets, 0 )
            else
                ( game.bullets, Basics.min (game.ship.bullets + 1) 6 )
    in
        ( newBullets
            |> List.filter (liveBullet game.tick)
            |> List.map updateBullet
        , remaingBullets
        )


liveBullet : Int -> Model.Bullet -> Bool
liveBullet tick { fired } =
    tick < fired + 50


updateBullet : Model.Bullet -> Model.Bullet
updateBullet bullet =
    { bullet | pos = move bullet.pos bullet.velocity }


fireBullets : Model.Game -> List Model.Bullet
fireBullets { tick, ship, bullets } =
    let
        heading =
            degrees (toFloat ship.heading)

        noseOffset =
            v2p ( 15, heading )

        position : Model.Point
        position =
            { x = ship.pos.x + noseOffset.x, y = ship.pos.y + noseOffset.y }

        ( shipVx, shipVy ) =
            fromPolar ship.velocity

        ( launchVx, launchVy ) =
            fromPolar ( 10, heading )

        bulletV =
            toPolar ( shipVx + launchVx, shipVy + launchVy )

        newBullet : Model.Bullet
        newBullet =
            { fired = tick, pos = position, velocity = bulletV }
    in
        newBullet :: bullets


range : Model.Point -> Model.Point -> Float
range a b =
    sqrt (toFloat (((a.x - b.x) ^ 2) + ((a.y - b.y) ^ 2)))


bulletRockCollision : Model.Bullet -> Model.Rock -> Bool
bulletRockCollision bullet rock =
    range bullet.pos rock.pos <= 1 + toFloat rock.radius



{- Assumption - a bullet will only hit one rock - not proven -}


bulletHit : List Model.Rock -> Model.Bullet -> List ( Model.Bullet, Model.Rock )
bulletHit rocks bullet =
    List.map (\r -> ( bullet, r )) (List.filter (bulletRockCollision bullet) rocks)


explodeRocks : Model.Rock -> ( Int, List Model.Rock )
explodeRocks { pos, velocity, radius } =
    let
        ( r, t ) =
            velocity

        r_ =
            r + 0.3

        rad =
            round (toFloat radius / 2.0)
    in
        if radius > 8 then
            ( 1
            , [ (Model.Rock { x = pos.x, y = pos.y } ( r_, t + 0.5 ) rad)
              , (Model.Rock { x = pos.x, y = pos.y } ( r_, t - 0.5 ) rad)
              ]
            )
        else
            ( 2, [] )


bulletHits : List Model.Bullet -> List Model.Rock -> ( List Model.Bullet, List Model.Rock, Model.Score )
bulletHits bullets rocks =
    let
        hits =
            List.concat (List.map (bulletHit rocks) bullets)

        ( bHits, rHits ) =
            List.unzip hits

        ( scores, rockFragments ) =
            List.unzip (List.map explodeRocks rHits)

        score =
            List.sum scores

        newBullets =
            List.filter (\b -> not (List.member b bHits)) bullets

        newRocks =
            List.filter (\r -> not (List.member r rHits)) rocks
    in
        ( newBullets, newRocks ++ (List.concat rockFragments), score )


shipImpact : Model.Ship -> List Model.Rock -> Bool
shipImpact ship rocks =
    List.any (\r -> (range ship.pos r.pos) <= toFloat (r.radius + 15)) rocks


message : msg -> Cmd msg
message x =
    Task.perform identity identity (Task.succeed x)
