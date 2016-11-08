module View exposing (view)

import Model
import Html
import Svg exposing (Svg)
import Svg.Attributes exposing (..)


view : Model.Model -> Html.Html Model.Msg
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


startScreen : Html.Html Model.Msg
startScreen =
    Svg.svg
        (svgScreen Model.screenWidth Model.screenHeight)
        ([ background ]
            ++ showMsg "Asteroids!" (Model.Point Model.midX (Model.midY - 60)) 60
            ++ showMsg "Press 'B' to start" (Model.Point Model.midX (Model.midY + 32)) 20
        )


endScreen : Int -> Html.Html Model.Msg
endScreen score =
    Svg.svg
        (svgScreen Model.screenWidth Model.screenHeight)
        ([ background ]
            ++ showMsg "Game Over!" (Model.Point Model.midX (Model.midY - 60)) 60
            ++ showMsg (toString score) (Model.Point Model.midX (Model.midY + 32)) 32
            ++ showMsg "Press 'B' to start again" (Model.Point Model.midX (Model.midY + 80)) 20
        )


scene : Model.Round -> Int -> Html.Html Model.Msg
scene currentRound lives =
    Svg.svg
        (svgScreen Model.screenWidth Model.screenHeight)
        ([ background
         , myShip currentRound.ship
         ]
            ++ List.map bullet currentRound.bullets
            ++ List.map simpleRock currentRound.rocks
            ++ remainingLives lives
            ++ showMsg (toString currentRound.score) (Model.Point ((Model.screenWidth * 2) // 3) 32) 32
        )


showMsg : String -> Model.Point -> Int -> List (Svg Model.Msg)
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


remainingLives : Int -> List (Svg Model.Msg)
remainingLives lives =
    let
        lifePos =
            List.map (\x -> { x = 32 * x, y = 20 }) [1..lives]
    in
        List.map (aShip 270) lifePos


background : Svg Model.Msg
background =
    Svg.rect
        [ width (toString Model.screenWidth)
        , height (toString Model.screenHeight)
        , fill "black"
        ]
        []


simpleRock : Model.Rock -> Svg Model.Msg
simpleRock rock =
    Svg.circle
        [ cx (toString rock.pos.x)
        , cy (toString rock.pos.y)
        , r (toString rock.radius)
        , stroke "white"
        , fillOpacity "0.0"
        ]
        []


bullet : Model.Bullet -> Svg Model.Msg
bullet bullet =
    Svg.circle
        [ cx (toString bullet.pos.x)
        , cy (toString bullet.pos.y)
        , r "1"
        , stroke "red"
        , fill "red"
        ]
        []


myShip : Model.Ship -> Svg Model.Msg
myShip ship =
    aShip ship.heading ship.pos


aShip : Int -> Model.Point -> Svg Model.Msg
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
