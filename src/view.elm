module View exposing (view)

import Model
import Html
import Html.Attributes as Ha
import Html.Events as Evts
import Svg exposing (Svg)
import Svg.Attributes exposing (..)


type alias Coords =
    ( Float, Float )


rockPts : List Coords
rockPts =
    [ ( 0.1, 1 ), ( 0.4, 0.7 ), ( 0.4, 0.6 ), ( 0.5, 0.6 ), ( 0.6, 0.5 ), ( 0.6, 0.4 ), ( 0.5, 0.3 ), ( 0.6, 0.2 ), ( 0.9, 0.2 ), ( 1, 0.1 ), ( 1, -0.2 ), ( 0.9, -0.3 ), ( 0.8, -0.3 ), ( 0.5, -0.6 ), ( 0.5, -0.7 ), ( 0.4, -0.6 ), ( 0.3, -0.7 ), ( 0.2, -0.9 ), ( 0.2, -1 ), ( 0.1, -1 ), ( -0.1, -0.9 ), ( -0.2, -0.8 ), ( -0.2, -0.7 ), ( -0.3, -0.6 ), ( -0.3, -0.6 ), ( -0.4, -0.7 ), ( -0.5, -0.5 ), ( -0.7, -0.4 ), ( -0.7, -0.3 ), ( -0.8, -0.2 ), ( -0.9, -0.1 ), ( -1, 0.1 ), ( -1, 0.2 ), ( -0.8, 0.4 ), ( -0.7, 0.5 ), ( -0.6, 0.5 ), ( -0.5, 0.4 ), ( -0.5, 0.3 ), ( -0.4, 0.2 ), ( -0.3, 0.3 ), ( -0.3, 0.4 ), ( -0.2, 0.5 ), ( -0.2, 0.6 ), ( -0.3, 0.7 ), ( -0.2, 0.8 ), ( -0.2, 0.9 ), ( -0.1, 1 ) ]


shipPts : List Coords
shipPts =
    [ ( -1.5, -1 ), ( 1.5, 0 ), ( -1.5, 1 ), ( -1, 0.6 ), ( -1, -0.6 ) ]


{-|
Primary view function
-}
view : Model.Model -> Html.Html Model.Msg
view model =
    case model.state of
        Model.Running currentRound ->
            Html.div []
                [ runningScene model.space currentRound model.lives
                , controls model.space
                  -- , Html.div [ class "debug" ] [ Html.text (toString currentRound.ship) ]
                ]

        Model.Start ->
            instructionScreen (startScreen model.space)

        Model.GameOver score ->
            instructionScreen ((endScreen model.space) score model.highScore)


controls : Model.GameSpace -> Html.Html Model.Msg
controls win =
    Html.div []
        [ Html.div
            [ class "controls"
            , Ha.style
                [ ( "width", toString win.maxX )
                ]
            ]
            [ Html.button
                [ class "ctrl"
                , Evts.onMouseDown (Model.Action Model.RotateLeft)
                , Evts.onMouseUp (Model.Action Model.MaintainHeading)
                ]
                [ Html.text "Left" ]
            , Html.button
                [ class "ctrl"
                , Evts.onMouseDown (Model.Action Model.Accelerate)
                , Evts.onMouseUp (Model.Action Model.Coast)
                ]
                [ Html.text "Fwd" ]
            , Html.button
                [ class "ctrl"
                , Evts.onMouseDown (Model.Action Model.RotateLeft)
                , Evts.onMouseUp (Model.Action Model.MaintainHeading)
                ]
                [ Html.text "Right" ]
            ]
        , Html.div [ class "controls" ]
            [ Html.button
                [ class "ctrl"
                , Evts.onMouseDown (Model.Action Model.Fire)
                , Evts.onMouseUp (Model.Action Model.CeaseFire)
                ]
                [ Html.text "Fire" ]
            ]
        ]


instructionScreen : Html.Html Model.Msg -> Html.Html Model.Msg
instructionScreen screen =
    Html.div []
        [ screen
        , Html.div [ class "controls" ]
            [ Html.button
                [ class "button"
                , Evts.onMouseDown Model.NewRound
                ]
                [ Html.text "Start" ]
            ]
        ]


startScreen : Model.GameSpace -> Html.Html Model.Msg
startScreen space =
    let
        { maxX, maxY, midX, midY } =
            space
    in
        Svg.svg
            (svgArea maxX maxY)
            ([ drawBackground space ]
                ++ drawMsg "Asteroids!" (Model.Point midX (midY - 60)) 60
                ++ drawMsg "Press 'B' to start" (Model.Point midX (midY + 32)) 16
                ++ drawMsg "A - rotate anti-clockwise" (Model.Point midX (midY + 47)) 10
                ++ drawMsg "D - rotate clockwise" (Model.Point midX (midY + 62)) 10
                ++ drawMsg "W - forward" (Model.Point midX (midY + 77)) 10
                ++ drawMsg "Spc - Fire" (Model.Point midX (midY + 92)) 10
            )


endScreen : Model.GameSpace -> Int -> Int -> Html.Html Model.Msg
endScreen win score highScore =
    let
        { maxX, maxY, midX, midY } =
            win
    in
        Svg.svg
            (svgArea win.maxX win.maxY)
            ([ (drawBackground win) ]
                ++ drawMsg "Game Over!" (Model.Point midX (midY - 60)) 60
                ++ drawMsg ("Score - " ++ (toString score)) (Model.Point midX (midY + 32)) 28
                ++ drawMsg ("High Score - " ++ (toString highScore)) (Model.Point midX (midY + 64)) 28
                ++ drawMsg "Press 'B' to start again" (Model.Point midX (midY + 94)) 16
            )


runningScene : Model.GameSpace -> Model.Round -> Int -> Html.Html Model.Msg
runningScene win currentRound lives =
    Svg.svg
        (svgArea win.maxX win.maxY)
        ([ (drawBackground win)
         , drawShip currentRound.ship
         ]
            ++ List.map drawBullet currentRound.bullets
            ++ List.map drawRock currentRound.rocks
            ++ remainingLives lives
            ++ drawMsg (toString currentRound.score) (Model.Point ((win.maxX * 2) // 3) 32) 32
        )


svgArea : Int -> Int -> List (Svg.Attribute msg)
svgArea x y =
    [ width (toString x)
    , height (toString y)
    ]


remainingLives : Int -> List (Svg Model.Msg)
remainingLives lives =
    let
        drawLifePos =
            List.map (\x -> { x = 32 * x, y = 20 }) (List.range 1 lives)
    in
        List.map drawLife drawLifePos


drawMsg : String -> Model.Point -> Int -> List (Svg Model.Msg)
drawMsg msg pos size =
    let
        styleText =
            "font-family: sans-serif; font-size: " ++ (toString size) ++ "pt; text-anchor: middle"
    in
        [ Svg.text_
            [ x (toString pos.x)
            , y (toString pos.y)
            , style styleText
            , stroke "none"
            , fill "white"
            ]
            [ Html.text msg ]
        ]


drawBackground : Model.GameSpace -> Svg Model.Msg
drawBackground { maxX, maxY } =
    Svg.rect
        [ width (toString maxX)
        , height (toString maxY)
        , fill "black"
        ]
        []


drawBullet : Model.Bullet -> Svg Model.Msg
drawBullet drawBullet =
    Svg.circle
        [ cx (toString drawBullet.navStat.pos.x)
        , cy (toString drawBullet.navStat.pos.y)
        , r "1"
        , stroke "red"
        , fill "red"
        ]
        []


scaleCoords : Int -> Coords -> String
scaleCoords s ( x, y ) =
    toString (x * toFloat s) ++ " " ++ toString (y * toFloat s)


drawObject : Model.Point -> Int -> Int -> List Coords -> String -> Svg Model.Msg
drawObject pos scale angle rawPts strokeColor =
    Svg.polygon
        [ points (List.foldl (++) "" (List.intersperse "," (List.map (scaleCoords scale) rawPts)))
        , stroke strokeColor
        , fillOpacity "0.0"
        , transform
            ("rotate("
                ++ (toString angle)
                ++ ","
                ++ (toString pos.x)
                ++ ","
                ++ (toString pos.y)
                ++ ")"
                ++ " "
                ++ "translate("
                ++ (toString pos.x)
                ++ ","
                ++ (toString pos.y)
                ++ ")"
            )
        ]
        []


drawRock : Model.Rock -> Svg Model.Msg
drawRock { navStat } =
    drawObject navStat.pos navStat.radius navStat.heading rockPts "blue"


drawShip : Model.Ship -> Svg Model.Msg
drawShip { navStat } =
    drawObject navStat.pos 10 navStat.heading shipPts "white"


drawLife : Model.Point -> Svg Model.Msg
drawLife pos =
    drawObject pos 10 270 shipPts "white"
