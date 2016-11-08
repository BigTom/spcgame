module View exposing (view)

import Model
    exposing
        ( Model
        , Round
        , Point
        , Ship
        , Msg
        , Bullet
        , Rock
        , screenWidth
        , screenHeight
        , midX
        , midY
        )
import Html
import Svg exposing (Svg)
import Svg.Attributes exposing (..)


view : Model -> Html.Html Msg
view model =
    case model.state of
        Model.Running currentRound ->
            Html.div []
                [ scene currentRound model.lives
                , Html.div [] [ Html.text (toString currentRound.ship) ]
                ]

        Model.Start ->
            Html.div [] [ startScreen ]

        Model.Over score ->
            Html.div [] [ endScreen score ]


svgScreen : Int -> Int -> List (Svg.Attribute msg)
svgScreen x y =
    [ width (toString x)
    , height (toString y)
    , style ("margin-left: 20 px")
    ]


startScreen : Html.Html Msg
startScreen =
    Svg.svg
        (svgScreen screenWidth screenHeight)
        ([ background ]
            ++ showMsg "Asteroids!" (Point midX (midY - 60)) 60
            ++ showMsg "Press 'B' to start" (Point midX (midY + 32)) 20
        )


endScreen : Int -> Html.Html Msg
endScreen score =
    Svg.svg
        (svgScreen screenWidth screenHeight)
        ([ background ]
            ++ showMsg "Game Over!" (Point midX (midY - 60)) 60
            ++ showMsg (toString score) (Point midX (midY + 32)) 32
            ++ showMsg "Press 'B' to start again" (Point midX (midY + 80)) 20
        )


scene : Round -> Int -> Html.Html Msg
scene currentRound lives =
    Svg.svg
        (svgScreen screenWidth screenHeight)
        ([ background
         , myShip currentRound.ship
         ]
            ++ List.map bullet currentRound.bullets
            ++ List.map simpleRock currentRound.rocks
            ++ remainingLives lives
            ++ showMsg (toString currentRound.score) (Point ((screenWidth * 2) // 3) 32) 32
        )


showMsg : String -> Model.Point -> Int -> List (Svg Msg)
showMsg msg pos size =
    let
        styleText =
            "font-family: sans-serif; font-size: " ++ (toString size) ++ "pt; text-anchor: middle"
    in
        [ Svg.text'
            [ x (toString pos.x)
            , y (toString pos.y)
            , style styleText
            , stroke "none"
            , fill "white"
            ]
            [ Html.text msg ]
        ]


remainingLives : Int -> List (Svg Msg)
remainingLives lives =
    let
        lifePos =
            List.map (\x -> { x = 32 * x, y = 20 }) [1..lives]
    in
        List.map (aShip 270) lifePos


background : Svg Msg
background =
    Svg.rect
        [ width (toString screenWidth)
        , height (toString screenHeight)
        , fill "black"
        ]
        []


simpleRock : Rock -> Svg Msg
simpleRock rock =
    Svg.circle
        [ cx (toString rock.pos.x)
        , cy (toString rock.pos.y)
        , r (toString rock.radius)
        , stroke "white"
        , fillOpacity "0.0"
        ]
        []


bullet : Bullet -> Svg Msg
bullet bullet =
    Svg.circle
        [ cx (toString bullet.pos.x)
        , cy (toString bullet.pos.y)
        , r "1"
        , stroke "red"
        , fill "red"
        ]
        []


myShip : Ship -> Svg Msg
myShip ship =
    aShip ship.heading ship.pos


aShip : Int -> Point -> Svg Msg
aShip heading { x, y } =
    let
        pts =
            toString (x - 15)
                ++ " "
                ++ toString (y - 10)
                ++ ", "
                ++ toString
                    (x + 15)
                ++ " "
                ++ toString y
                ++ ", "
                ++ toString
                    (x - 15)
                ++ " "
                ++ toString (y + 10)
                ++ ", "
                ++ toString
                    (x - 10)
                ++ " "
                ++ toString (y + 6)
                ++ ", "
                ++ toString
                    (x - 10)
                ++ " "
                ++ toString (y - 6)
    in
        Svg.polygon
            [ points pts
              -- , fill "white"
            , stroke "white"
            , transform
                ("rotate("
                    ++ (toString heading)
                    ++ ","
                    ++ (toString x)
                    ++ ","
                    ++ (toString y)
                    ++ ")"
                )
            ]
            []
