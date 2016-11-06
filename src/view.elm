module View exposing (..)

import Model exposing (Model, Ship, Msg, Bullet, Rock, screenWidth, screenHeight)
import Html
import Svg exposing (Svg)
import Svg.Attributes exposing (..)


view : Model -> Html.Html Msg
view model =
    Html.div []
        [ scene model.currentGame
        , Html.div [] [ Html.text (toString model.currentGame.ship) ]
        ]


scene : Model.Game -> Html.Html Msg
scene game =
    Svg.svg
        [ width (toString screenWidth)
        , height (toString screenHeight)
        , style ("margin-left: 20 px")
        ]
        ([ background
         , myShip game.ship
         ]
            ++ List.map bullet game.bullets
            ++ List.map simpleRock game.rocks
        )


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
        , stroke "white"
        , fill "white"
        ]
        []


myShip : Ship -> Svg Msg
myShip ship =
    let
        { x, y } =
            ship.pos

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
                    ++ (toString ship.heading)
                    ++ ","
                    ++ (toString ship.pos.x)
                    ++ ","
                    ++ (toString ship.pos.y)
                    ++ ")"
                )
            ]
            []
