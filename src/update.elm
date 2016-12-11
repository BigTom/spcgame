module Update exposing (..)

import Model
import Window


update : Model.Msg -> Model.Model -> ( Model.Model, Cmd Model.Msg )
update msg model =
    case msg of
        Model.Tick _ ->
            case model.state of
                Model.Running round ->
                    onTick model round

                _ ->
                    ( model, Cmd.none )

        Model.NewRound ->
            Model.genGame model.highScore model.seed model.win

        Model.Action act ->
            case model.state of
                Model.Running round ->
                    ( onShipAction model round act, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        Model.Resize newWin ->
            onResize model newWin


onShipAction : Model.Model -> Model.Round -> Model.Act -> Model.Model
onShipAction model round act =
    let
        state =
            model.state

        ship =
            round.ship

        newShip =
            case act of
                Model.Accelerate ->
                    { ship | accelerating = True }

                Model.RotateLeft ->
                    { ship | rotating = Model.Clockwise }

                Model.RotateRight ->
                    { ship | rotating = Model.Anticlockwise }

                Model.Fire ->
                    { ship | firing = True }

                Model.Coast ->
                    { ship | accelerating = False }

                Model.MaintainHeading ->
                    { ship | rotating = Model.Not }

                Model.CeaseFire ->
                    { ship | firing = False }

                Model.None ->
                    ship

        newRound =
            { round | ship = newShip }
    in
        { model | state = Model.Running newRound }


onResize : Model.Model -> Window.Size -> ( Model.Model, Cmd Model.Msg )
onResize model { width, height } =
    let
        newWin =
            Debug.log "resize: "
                (Model.Win
                    width
                    (height - 100)
                    (width // 2)
                    ((height - 100) // 2)
                )
    in
        ( { model
            | win = newWin
          }
        , Cmd.none
        )


lostLife : Model.Model -> Model.Round -> Model.Model
lostLife model currentRound =
    if model.lives > 1 then
        let
            difficulty =
                1

            ( newRound, nextSeed ) =
                Model.genRound model.win difficulty currentRound.score model.seed
        in
            { model
                | state = Model.Running newRound
                , lives = model.lives - 1
                , difficulty = difficulty
                , seed = nextSeed
            }
    else
        let
            newHighScore =
                max model.highScore currentRound.score
        in
            { model
                | state = Model.GameOver currentRound.score
                , highScore = newHighScore
            }


wonLevel : Model.Model -> Model.Round -> Model.Model
wonLevel model currentRound =
    let
        difficulty =
            model.difficulty + 1

        ( newRound, nextSeed ) =
            Model.genRound model.win difficulty currentRound.score model.seed
    in
        { model
            | state = Model.Running newRound
            , difficulty = difficulty
            , seed = nextSeed
        }


onTick : Model.Model -> Model.Round -> ( Model.Model, Cmd Model.Msg )
onTick model game =
    let
        ( newBullets, remainingBullets ) =
            updateBullets model.win game

        ( newNewBullets, newRocks, score ) =
            bulletHits newBullets (updateRocks model.win game.rocks)

        newRound =
            { game
                | tick = game.tick + 1
                , ship = updateShip model.win game.ship remainingBullets
                , bullets = newNewBullets
                , rocks = newRocks
                , score = game.score + score
            }

        newModel =
            if shipImpact newRound.ship newRound.rocks then
                lostLife model newRound
            else if List.isEmpty newRocks then
                wonLevel model newRound
            else
                { model | state = Model.Running newRound }
    in
        ( newModel
        , Cmd.none
        )


v2p : Model.Velocity -> Model.Point
v2p velocity =
    (\( x, y ) -> { x = round x, y = round y })
        (fromPolar velocity)


screenWrap : Model.Win -> Model.Point -> Model.Point
screenWrap win coord =
    { x = coord.x % win.maxX, y = coord.y % win.maxY }


updateShip : Model.Win -> Model.Ship -> Int -> Model.Ship
updateShip win ship remainingBullets =
    let
        newVelocity =
            if ship.accelerating then
                accelerateShip ship 0.5
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
            | pos = move win ship.pos ship.velocity
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


updateRocks : Model.Win -> List Model.Rock -> List Model.Rock
updateRocks win rocks =
    List.map (updateRock win) rocks


move : Model.Win -> Model.Point -> Model.Velocity -> Model.Point
move win { x, y } v =
    let
        d =
            v2p v
    in
        { x = (x + d.x), y = (y + d.y) } |> (screenWrap win)


updateRock : Model.Win -> Model.Rock -> Model.Rock
updateRock win rock =
    { rock | pos = move win rock.pos rock.velocity, angle = rock.angle + rock.rotation }


updateBullets : Model.Win -> Model.Round -> ( List Model.Bullet, Int )
updateBullets win game =
    let
        ( newBullets, remaingBullets ) =
            if game.ship.firing then
                if game.ship.bullets > 0 then
                    ( fireBullets game, game.ship.bullets - 3 )
                else
                    ( game.bullets, 0 )
            else
                ( game.bullets, Basics.min (game.ship.bullets + 1) 6 )
    in
        ( newBullets
            |> List.filter (liveBullet game.tick)
            |> List.map (updateBullet win)
        , remaingBullets
        )


liveBullet : Int -> Model.Bullet -> Bool
liveBullet tick { fired } =
    tick < fired + 30


updateBullet : Model.Win -> Model.Bullet -> Model.Bullet
updateBullet win bullet =
    { bullet | pos = move win bullet.pos bullet.velocity }


fireBullets : Model.Round -> List Model.Bullet
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
explodeRocks { pos, velocity, radius, rotation } =
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
            , [ Model.Rock { x = pos.x, y = pos.y } ( r_, t + 0.5 ) rad (rotation * 2) 0
              , Model.Rock { x = pos.x, y = pos.y } ( r_, t - 0.5 ) rad (rotation * 2) 0
              ]
            )
        else
            ( 2, [] )


bulletHits : List Model.Bullet -> List Model.Rock -> ( List Model.Bullet, List Model.Rock, Model.Score )
bulletHits bullets rocks =
    let
        hits =
            Debug.log "hits"
                List.concat
                (List.map (bulletHit rocks) bullets)

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
