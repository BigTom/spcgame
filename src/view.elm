module View exposing (view)

import Model
import Html
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


view : Model.Model -> Html.Html Model.Msg
view model =
    case model.state of
        Model.Running currentRound ->
            Html.div []
                [ runningScene currentRound model.lives
                , Html.div [] [ Html.text (toString currentRound.ship) ]
                ]

        Model.Start ->
            Html.div [] [ startScreen ]

        Model.Over score ->
            Html.div [] [ endScreen score ]


startScreen : Html.Html Model.Msg
startScreen =
    Svg.svg
        (svgArea Model.screenWidth Model.screenHeight)
        ([ drawBackground ]
            ++ drawMsg "Asteroids!" (Model.Point Model.midX (Model.midY - 60)) 60
            ++ drawMsg "Press 'B' to start" (Model.Point Model.midX (Model.midY + 32)) 20
        )


endScreen : Int -> Html.Html Model.Msg
endScreen score =
    Svg.svg
        (svgArea Model.screenWidth Model.screenHeight)
        ([ drawBackground ]
            ++ drawMsg "Game Over!" (Model.Point Model.midX (Model.midY - 60)) 60
            ++ drawMsg (toString score) (Model.Point Model.midX (Model.midY + 32)) 32
            ++ drawMsg "Press 'B' to start again" (Model.Point Model.midX (Model.midY + 80)) 20
        )


runningScene : Model.Round -> Int -> Html.Html Model.Msg
runningScene currentRound lives =
    Svg.svg
        (svgArea Model.screenWidth Model.screenHeight)
        ([ drawBackground
         , drawShip currentRound.ship
         ]
            ++ List.map drawBullet currentRound.bullets
            ++ List.map drawRock currentRound.rocks
            ++ remainingLives lives
            ++ drawMsg (toString currentRound.score) (Model.Point ((Model.screenWidth * 2) // 3) 32) 32
        )


svgArea : Int -> Int -> List (Svg.Attribute msg)
svgArea x y =
    [ width (toString x)
    , height (toString y)
    , style ("margin-left: 20 px")
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


drawBackground : Svg Model.Msg
drawBackground =
    Svg.rect
        [ width (toString Model.screenWidth)
        , height (toString Model.screenHeight)
        , fill "black"
        ]
        []


drawBullet : Model.Bullet -> Svg Model.Msg
drawBullet drawBullet =
    Svg.circle
        [ cx (toString drawBullet.pos.x)
        , cy (toString drawBullet.pos.y)
        , r "1"
        , stroke "red"
        , fill "red"
        ]
        []


scaleCoords : Int -> Coords -> String
scaleCoords s ( x, y ) =
    toString (x * toFloat s) ++ " " ++ toString (y * toFloat s)


drawObject : Model.Point -> Int -> Int -> List Coords -> Svg Model.Msg
drawObject pos scale angle rawPts =
    Svg.polygon
        [ points (List.foldl (++) "" (List.intersperse "," (List.map (scaleCoords scale) rawPts)))
        , stroke "white"
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
drawRock { pos, radius, angle } =
    drawObject pos radius angle rockPts


drawShip : Model.Ship -> Svg Model.Msg
drawShip ship =
    drawObject ship.pos 10 ship.heading shipPts


drawLife : Model.Point -> Svg Model.Msg
drawLife pos =
    drawObject pos 10 270 shipPts
