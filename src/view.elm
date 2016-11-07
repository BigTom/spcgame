module View exposing (..)

import Model exposing (Model, Game, Point, Ship, Msg, Bullet, Rock, screenWidth, screenHeight)
import Html
import Svg exposing (Svg)
import Svg.Attributes exposing (..)


view : Model -> Html.Html Msg
view model =
    if model.lives > 0 then
        Html.div []
            [ scene model
            , Html.div [] [ Html.text (toString model.currentGame.ship) ]
            ]
    else
        Html.div []
            [ endScreen model ]


endScreen : Model -> Html.Html Msg
endScreen { currentGame } =
    Svg.svg
        [ width (toString screenWidth)
        , height (toString screenHeight)
        , style ("margin-left: 20 px")
        ]
        ([ background ]
            ++ showScore currentGame.score
        )


scene : Model -> Html.Html Msg
scene { currentGame, lives } =
    Svg.svg
        [ width (toString screenWidth)
        , height (toString screenHeight)
        , style ("margin-left: 20 px")
        ]
        ([ background
         , myShip currentGame.ship
         ]
            ++ List.map bullet currentGame.bullets
            ++ List.map simpleRock currentGame.rocks
            ++ remainingLives lives
            ++ showScore currentGame.score
        )


showScore : Int -> List (Svg Msg)
showScore sc =
    [ Svg.text'
        [ x (toString ((screenWidth * 2) // 3))
        , y "32"
        , style "font-family: sans-serif; font-size: 30pt"
        , stroke "none"
        , fill "white"
        ]
        [ Html.text (toString sc) ]
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
