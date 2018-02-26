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

        Model.Action act ->
            case model.state of
                Model.Running round ->
                    ( onShipAction model round act, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        Model.NewRound ->
            Model.genGame model.highScore model.seed model.space

        Model.Resize newWin ->
            onResize model newWin


onTick : Model.Model -> Model.Round -> ( Model.Model, Cmd Model.Msg )
onTick model game =
    let
        ( newBullets, remainingBullets ) =
            updateBullets model.space game

        ( newNewBullets, newRocks, score ) =
            bulletHits newBullets (updateRocks model.space game.rocks)

        newRound =
            { tick = game.tick + 1
            , ship = updateShip model.space game.ship remainingBullets
            , bullets = newNewBullets
            , rocks = newRocks
            , score = game.score + score
            }

        newModel =
            if impact newRound.ship.navStat (List.map .navStat newRound.rocks) then
                lostLife model newRound
            else if List.isEmpty newRocks then
                wonLevel model newRound
            else
                { model | state = Model.Running newRound }
    in
        ( newModel
        , Cmd.none
        )


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
        newSpace =
            Debug.log "resize: "
                (Model.GameSpace
                    width
                    (height - 100)
                    (width // 2)
                    ((height - 100) // 2)
                )
    in
        ( { model
            | space = newSpace
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
                Model.genRound model.space difficulty currentRound.score model.seed
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
            Model.genRound model.space difficulty currentRound.score model.seed
    in
        { model
            | state = Model.Running newRound
            , difficulty = difficulty
            , seed = nextSeed
        }


v2p : Model.Velocity -> Model.Point
v2p velocity =
    (\( x, y ) -> { x = round x, y = round y })
        (fromPolar velocity)


screenWrap : Model.GameSpace -> Model.Point -> Model.Point
screenWrap space coord =
    { x = coord.x % space.maxX, y = coord.y % space.maxY }


updateShip : Model.GameSpace -> Model.Ship -> Int -> Model.Ship
updateShip space ship remainingBullets =
    let
        navStat =
            ship.navStat

        newNavStat =
            if ship.accelerating then
                accelerateShip navStat 0.5
            else
                accelerateShip navStat 0.0

        rotation =
            case ship.rotating of
                Model.Clockwise ->
                    3

                Model.Anticlockwise ->
                    -3

                Model.Not ->
                    0
    in
        { ship
            | navStat = move space newNavStat rotation
            , bullets = remainingBullets
        }


accelerateShip : Model.NavStat -> Float -> Model.NavStat
accelerateShip navStat acc =
    let
        ( vx, vy ) =
            fromPolar navStat.velocity

        ( dx, dy ) =
            fromPolar ( acc, degrees (toFloat navStat.heading) )
    in
        { navStat | velocity = toPolar ( vx + dx, vy + dy ) }


updateRocks : Model.GameSpace -> List Model.Rock -> List Model.Rock
updateRocks space rocks =
    let
        updateRock : Model.GameSpace -> Model.Rock -> Model.Rock
        updateRock space rock =
            { rock | navStat = move space rock.navStat rock.rotation }
    in
        List.map (updateRock space) rocks


move : Model.GameSpace -> Model.NavStat -> Int -> Model.NavStat
move space navStat rotation =
    let
        { pos, velocity, radius, heading } =
            navStat

        d =
            v2p velocity

        newPos =
            { x = (pos.x + d.x), y = (pos.y + d.y) } |> (screenWrap space)
    in
        { navStat
            | pos = newPos
            , heading = navStat.heading + rotation
        }


updateBullets : Model.GameSpace -> Model.Round -> ( List Model.Bullet, Int )
updateBullets space game =
    let
        ( newBullets, remaingBullets ) =
            if game.ship.firing then
                if game.ship.bullets > 0 then
                    ( fireBullets game, game.ship.bullets - 3 )
                else
                    ( game.bullets, 0 )
            else
                ( game.bullets
                , Basics.min (game.ship.bullets + 1) 6
                )

        moveBullet : Model.GameSpace -> Model.Bullet -> Model.Bullet
        moveBullet space bullet =
            { bullet | navStat = move space bullet.navStat 0 }

        liveBullet : Int -> Model.Bullet -> Bool
        liveBullet tick { fired } =
            tick < fired + 30
    in
        ( newBullets
            |> List.filter (liveBullet game.tick)
            |> List.map (moveBullet space)
        , remaingBullets
        )


fireBullets : Model.Round -> List Model.Bullet
fireBullets { tick, ship, bullets } =
    let
        heading =
            degrees (toFloat ship.navStat.heading)

        noseOffset =
            v2p ( 15, heading )

        position : Model.Point
        position =
            { x = ship.navStat.pos.x + noseOffset.x, y = ship.navStat.pos.y + noseOffset.y }

        ( shipVx, shipVy ) =
            fromPolar ship.navStat.velocity

        ( launchVx, launchVy ) =
            fromPolar ( 10, heading )

        bulletV =
            toPolar ( shipVx + launchVx, shipVy + launchVy )

        newBullet : Model.Bullet
        newBullet =
            { fired = tick
            , navStat = { pos = position, velocity = bulletV, radius = 1, heading = 0 }
            }
    in
        newBullet :: bullets


range : Model.Point -> Model.Point -> Float
range a b =
    sqrt (toFloat (((a.x - b.x) ^ 2) + ((a.y - b.y) ^ 2)))


bulletRockCollision : Model.NavStat -> Model.Rock -> Bool
bulletRockCollision bNav { navStat, rotation } =
    range bNav.pos navStat.pos <= toFloat (bNav.radius + navStat.radius)



{- Assumption - a bullet will only hit one rock - not proven -}


bulletHit : List Model.Rock -> Model.Bullet -> List ( Model.Bullet, Model.Rock )
bulletHit rocks bullet =
    List.map (\r -> ( bullet, r )) (List.filter (bulletRockCollision bullet.navStat) rocks)


explodeRocks : Model.Rock -> ( Int, List Model.Rock )
explodeRocks { navStat, rotation } =
    let
        { pos, velocity } =
            navStat

        ( r, t ) =
            velocity

        r_ =
            r + 0.3

        rad =
            round (toFloat navStat.radius / 2.0)
    in
        if navStat.radius > 8 then
            ( 1
            , [ Model.Rock
                    (Model.NavStat { x = pos.x, y = pos.y }
                        ( r_, t + 0.5 )
                        rad
                        0
                    )
                    (rotation * 2)
              , Model.Rock
                    (Model.NavStat { x = pos.x, y = pos.y }
                        ( r_, t - 0.5 )
                        rad
                        0
                    )
                    (rotation * 2)
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


impact : Model.NavStat -> List Model.NavStat -> Bool
impact obj tgts =
    List.any (\t -> (range obj.pos t.pos) <= toFloat (t.radius + obj.radius)) tgts
