module Tests exposing (..)

import Model
import Update
import Test exposing (..)
import Expect


testShip : Model.Ship
testShip =
    { pos = { x = 100, y = 100 }
    , velocity = ( 0, degrees (toFloat 270) )
    , heading = 270
    , rotating = Model.Not
    , accelerating = False
    , firing = False
    , bullets = 6
    }


all : Test
all =
    describe "Update Tests"
        [ describe "v2p Test Suite"
            [ test "v2p 10, 90" <|
                \() ->
                    Expect.equal (Update.v2p ( 10, degrees 90 )) { x = 0, y = 10 }
            , test "v2p 10, 0" <|
                \() ->
                    Expect.equal (Update.v2p ( 10, degrees 0 )) { x = 10, y = 0 }
            , test "v2p 10, 10" <|
                \() ->
                    Expect.equal (Update.v2p ( 10, degrees 45 )) { x = 7, y = 7 }
            ]
        , describe "shipImpact Tests"
            [ test "hit" <|
                \() ->
                    Expect.true "true"
                        (Update.shipImpact testShip
                            [ Model.Rock
                                { x = 131, y = 100 }
                                ( 0, 0 )
                                15
                            , Model.Rock
                                { x = 129, y = 100 }
                                ( 0, 0 )
                                15
                            ]
                        )
            , test "miss" <|
                \() ->
                    Expect.false "false"
                        (Update.shipImpact testShip [ Model.Rock { x = 131, y = 100 } ( 0, 0 ) 15 ])
            ]
        ]
