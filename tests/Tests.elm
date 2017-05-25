module Tests exposing (..)

import Haggis.Card exposing (..)
import Haggis.Combination exposing (..)
import Test exposing (..)
import Expect exposing (..)


all =
    describe "Haggis"
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
                Expect.lessThan (rank redSeven) (rank blueTwo)
        , test "two cards with the same rank are equal" <|
            \() ->
                Expect.equal (rank blueTwo) (rank greenTwo)
        , test "non-matching single cards do not match" <|
            \() ->
                Expect.notEqual blueTwo redSeven
        , test "one card is a single" <|
            \() ->
                Expect.equal (set [ redSeven ]) (Just Single)
        , test "two cards of matching rank are a double" <|
            \() ->
                Expect.equal (set [ blueTwo, greenTwo ]) (Just Double)
        , test "two cards with unmatched ranks are not a combination" <|
            \() ->
                Expect.equal (set [ blueTwo, redSeven ]) Nothing
        , test "two matched number cards with one wild is a triple" <|
            \() ->
                Expect.equal (set [ blueTwo, greenTwo, jack ]) (Just Triple)
        , test "two wild cards is NOT a double" <|
            \() ->
                Expect.notEqual (set [ jack, queen ]) (Just Double)
        , test "one wild card is a single" <|
            \() ->
                Expect.equal (set [ jack ]) (Just Single)
        , test "two matched spot cards plus three wilds is a Quintuple" <|
            \() ->
                Expect.equal (set [ blueTwo, greenTwo, jack, queen, king ]) (Just Quintuple)
        , test "one wild card is not a bomb" <|
            \() ->
                Expect.equal (bomb [ king ]) Nothing
        , test "two wild cards is a bomb" <|
            \() ->
                Expect.equal (bomb [ queen, king ]) (Just QK)
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
        , test "four distinct odd cards, with 2-3 suits, does not have 4 suits" <|
            \() ->
                Expect.equal (hasFourSuits [ blueThree, redFive, redSeven, yellowNine ]) False
        , test "four distinct odd cards, with 2-3 suits, is a not bomb" <|
            \() ->
                Expect.equal (bomb [ blueThree, redFive, redSeven, yellowNine ]) Nothing
        , test "consecutive cards are all consecutive" <|
            \() ->
                Expect.equal (allMaybeConsecutive [ Just blueTwo, Just blueThree, Just blueFour ]) True
        , test "nonconsecutive cards are not all consecutive" <|
            \() ->
                Expect.equal (allMaybeConsecutive [ Just blueTwo, Just blueThree, Just redFive ]) True
        , test "An empty set of cards are not all consecutive" <|
            \() ->
                Expect.equal (allMaybeConsecutive []) False
        , test "A single card is all consecutive" <|
            \() ->
                Expect.equal (allMaybeConsecutive [ Just blueTwo ]) True
        , test "three consecutive singles is a sequence" <|
            \() ->
                Expect.equal (sequence [ blueTwo, blueThree, blueFour ]) (Just SingleRun)
        , test "three nonconsecutive singles is not a sequence" <|
            \() ->
                Expect.equal (sequence [ blueTwo, blueThree, redThree ]) Nothing
        , test "two consecutive singles is not a sequence" <|
            \() ->
                Expect.equal (sequence [ blueTwo, blueThree ]) Nothing
        , test "two consecutive doubles is a sequence" <|
            \() ->
                Expect.equal (sequence [ blueTwo, greenTwo, blueThree, greenThree ]) (Just DoubleRun)
        ]


blueTwo : Card
blueTwo =
    { suit = Blue, rank = Two, points = 0 }


greenTwo : Card
greenTwo =
    { suit = Green, rank = Two, points = 0 }


blueThree : Card
blueThree =
    { suit = Blue, rank = Three, points = 1 }


greenThree : Card
greenThree =
    { suit = Green, rank = Three, points = 1 }


redThree : Card
redThree =
    { suit = Red, rank = Three, points = 1 }


blueFour : Card
blueFour =
    { suit = Blue, rank = Four, points = 1 }


redFive : Card
redFive =
    { suit = Red, rank = Five, points = 1 }


greenFive : Card
greenFive =
    { suit = Green, rank = Five, points = 1 }


redSeven : Card
redSeven =
    { suit = Red, rank = Seven, points = 1 }


orangeSeven : Card
orangeSeven =
    { suit = Orange, rank = Seven, points = 1 }


redNine : Card
redNine =
    { suit = Red, rank = Nine, points = 1 }


yellowNine : Card
yellowNine =
    { suit = Yellow, rank = Nine, points = 1 }


jack : Card
jack =
    { suit = Wild, rank = Jack, points = 2 }


queen : Card
queen =
    { suit = Wild, rank = Queen, points = 3 }


king : Card
king =
    { suit = Wild, rank = King, points = 5 }
