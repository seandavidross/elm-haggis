module Tests exposing (..)

import Haggis.Card exposing (..)
import Haggis.Combination exposing (..)
import Test exposing (..)
import Expect exposing (..)


all =
    describe "Haggis"
        [ describe "Haggis.Card"
            [ test "new card has correct suit" <|
                \() ->
                    Expect.equal blueTwo.suit Blue
            , test "new card has correct rank" <|
                \() ->
                    Expect.equal blueTwo.rank Two
            , test "new card has correct points" <|
                \() ->
                    Expect.equal redSeven.points 1
            , test "card with lower rank less than card with higher rank" <|
                \() ->
                    Expect.lessThan redSeven.order blueTwo.order
            , test "two cards with the same rank are equal" <|
                \() ->
                    Expect.equal blueTwo.order greenTwo.order
            , test "non-matching single cards do not match" <|
                \() ->
                    Expect.notEqual blueTwo redSeven
            ]
        , describe "Haggis.Combination"
            [ describe "Haggis.Combination.Set"
                [ test "one card is a single" <|
                    \() ->
                        Expect.equal (set [ redSeven ]) (Just Single)
                , test "two cards of matching rank are a pair" <|
                    \() ->
                        Expect.equal (set [ blueTwo, greenTwo ]) (Just Pair)
                , test "two cards with unmatched ranks are not a combination" <|
                    \() ->
                        Expect.equal (set [ blueTwo, redSeven ]) Nothing
                , test "two matched number cards with one wild is a triple" <|
                    \() ->
                        Expect.equal (set [ blueTwo, greenTwo, jack ]) (Just Triple)
                , test "two wild cards is NOT a pair" <|
                    \() ->
                        Expect.notEqual (set [ jack, queen ]) (Just Pair)
                , test "one wild card is a single" <|
                    \() ->
                        Expect.equal (set [ jack ]) (Just Single)
                , test "two matched spot cards plus three wilds is a five-of-a-kind" <|
                    \() ->
                        Expect.equal (set [ blueTwo, greenTwo, jack, queen, king ]) (Just FiveOfAKind)
                , test "a pair of tens plus two wilds is a four-of-a-kind" <|
                    \() ->
                        Expect.equal (set [ blueTen, greenTen, jack, king ]) (Just FourOfAKind)
                ]
            , describe "Haggis.Combination.Bomb"
                [ test "one wild card is not a bomb" <|
                    \() ->
                        Expect.equal (bomb [ king ]) Nothing
                , test "two wild cards is a bomb" <|
                    \() ->
                        Expect.equal (bomb [ queen, king ]) (Just QK)
                , test "two wild cards is a bomb, order does not matter" <|
                    \() ->
                        Expect.equal (bomb [ king, jack ]) (Just JK)
                , test "three wild cards is a bomb" <|
                    \() ->
                        Expect.equal (bomb [ jack, queen, king ]) (Just JQK)
                , test "four distinct, same-suited odd cards is a bomb" <|
                    \() ->
                        Expect.equal (bomb [ redThree, redFive, redSeven, redNine ]) (Just Suited)
                , test "A suited bomb is a suited bomb regardless of card order" <|
                    \() ->
                        Expect.equal (bomb [ redFive, redNine, redThree, redSeven ]) (Just Suited)
                , test "four distinct odd cards, with distinct suits, is a bomb" <|
                    \() ->
                        Expect.equal (bomb [ blueThree, greenFive, redSeven, yellowNine ]) (Just Rainbow)
                , test "four distinct odd cards, with 2-3 suits, is a not bomb" <|
                    \() ->
                        Expect.equal (bomb [ blueThree, redFive, redSeven, yellowNine ]) Nothing
                ]
            , describe "Haggis.Combination.Sequence"
                [ test "empty set of cards is not a sequence" <|
                    \() ->
                        Expect.equal (sequence []) [ Nothing ]
                , test "three consecutive singles is a sequence" <|
                    \() ->
                        Expect.equal (sequence [ blueTwo, blueThree, blueFour ]) [ Just RunOfSingles ]
                , test "three nonconsecutive singles is not a sequence" <|
                    \() ->
                        Expect.equal (sequence [ blueTwo, blueThree, redThree ]) [ Nothing ]
                , test "two consecutive singles is not a sequence" <|
                    \() ->
                        Expect.equal (sequence [ blueTwo, blueThree ]) [ Nothing ]
                , test "three wildcards is not a sequence" <|
                    \() ->
                        Expect.equal (sequence [ jack, queen, king ]) [ Nothing ]
                , test "two consecutive pairs is a sequence" <|
                    \() ->
                        Expect.equal (sequence [ blueTwo, greenTwo, blueThree, greenThree ]) [ Just RunOfPairs ]
                , test "card order should not affect sequence identification" <|
                    \() ->
                        Expect.equal (sequence [ greenTwo, blueThree, greenThree, blueTwo ]) [ Just RunOfPairs ]
                , test "one spot card and two wildcards is a run of singles" <|
                    \() ->
                        Expect.equal (sequence [ blueTwo, jack, king ]) [ Just RunOfSingles ]
                , test "a ten and three wildcards is a run of singles" <|
                    \() ->
                        Expect.equal (sequence [ blueTen, jack, queen, king ]) [ Just RunOfSingles ]
                , test "one wildcard can fill a one rank gap between two singles to form a run" <|
                    \() ->
                        Expect.equal (sequence [ blueTwo, blueFour, king ]) [ Just RunOfSingles ]
                , test "one wildcard CANNOT fill a 2+ rank gap between two singles to form a run" <|
                    \() ->
                        Expect.equal (sequence [ greenTwo, greenFive, king ]) [ Nothing ]
                , test "two wildcards can fill a 2 rank gap between two singles to form a run" <|
                    \() ->
                        Expect.equal (sequence [ greenTwo, greenFive, king, jack ]) [ Just RunOfSingles ]

                -- , test "one spot card and three wildcards could be a run of singles or a run of pairs" <|
                --     \() ->
                --         Expect.equal (sequence [ blueTwo, jack, queen, king ]) [ Just RunOfSingles, Just RunOfPairs ]
                , test "wild card should sub for missing card in run of pairs" <|
                    \() ->
                        Expect.equal (sequence [ blueTwo, greenTwo, jack, greenThree ]) [ Just RunOfPairs ]
                , test "a pair of tens plus two wilds is a run of pairs (T-T-J-J)" <|
                    \() ->
                        Expect.equal (sequence [ blueTen, greenTen, jack, king ]) [ Just RunOfPairs ]
                , test "can fill holes in run of pairs that is longer than 3 ranks" <|
                    \() ->
                        Expect.equal (sequence [ blueTwo, greenTwo, blueThree, greenThree, blueFour, greenFive, jack, king ]) [ Just RunOfPairs ]

                -- , test "a consecutive single and a pair of spot cards, plus 3 wildcards, could be a run of pairs or a run of triples" <|
                --     \() ->
                --         Expect.equal (sequence [ blueTwo, greenTwo, greenThree, jack, queen, king ]) [ Just RunOfPairs, Just RunOfTriples ]
                -- , test "two consecutive pairs and two wildcards could be a run of pairs or a run of triples" <|
                --     \() ->
                --         Expect.equal (sequence [ blueTwo, blueThree, greenTwo, greenThree, jack, queen ]) [ Just RunOfPairs, Just RunOfTriples ]
                ]
            ]
        ]


blueTwo : Card
blueTwo =
    { suit = Blue, rank = Two, order = 2, points = 0 }


greenTwo : Card
greenTwo =
    { suit = Green, rank = Two, order = 2, points = 0 }


blueThree : Card
blueThree =
    { suit = Blue, rank = Three, order = 3, points = 1 }


greenThree : Card
greenThree =
    { suit = Green, rank = Three, order = 3, points = 1 }


redThree : Card
redThree =
    { suit = Red, rank = Three, order = 3, points = 1 }


blueFour : Card
blueFour =
    { suit = Blue, rank = Four, order = 4, points = 1 }


redFive : Card
redFive =
    { suit = Red, rank = Five, order = 5, points = 1 }


greenFive : Card
greenFive =
    { suit = Green, rank = Five, order = 5, points = 1 }


redSeven : Card
redSeven =
    { suit = Red, rank = Seven, order = 7, points = 1 }


orangeSeven : Card
orangeSeven =
    { suit = Orange, rank = Seven, order = 7, points = 1 }


redNine : Card
redNine =
    { suit = Red, rank = Nine, order = 9, points = 1 }


yellowNine : Card
yellowNine =
    { suit = Yellow, rank = Nine, order = 9, points = 1 }


blueTen : Card
blueTen =
    { suit = Blue, rank = Ten, order = 10, points = 0 }


greenTen : Card
greenTen =
    { suit = Green, rank = Ten, order = 10, points = 0 }


jack : Card
jack =
    { suit = Wild, rank = Jack, order = 11, points = 2 }


queen : Card
queen =
    { suit = Wild, rank = Queen, order = 12, points = 3 }


king : Card
king =
    { suit = Wild, rank = King, order = 13, points = 5 }
