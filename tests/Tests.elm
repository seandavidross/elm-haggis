module Tests exposing (..)

import Expect exposing (..)
import Haggis.Card as Card exposing (..)
import Haggis.Cards as Cards exposing (..)
import Haggis.Combination as Combo exposing (..)
import Haggis.Hand as Hand exposing (..)
import Test exposing (..)


all : Test
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
                    Expect.equal (Card.points redSeven) 1
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
                        Expect.equal (set [ redSeven ]) (Just (Single Seven))
                , test "two cards of matching rank are a pair" <|
                    \() ->
                        Expect.equal (set [ blueTwo, greenTwo ]) (Just (Pair Two))
                , test "two cards with unmatched ranks are not a combination" <|
                    \() ->
                        Expect.equal (set [ blueTwo, redSeven ]) Nothing
                , test "two matched number cards with one wild is a triple" <|
                    \() ->
                        Expect.equal
                            (set [ blueTwo, greenTwo, jack ])
                            (Just (Triple Two))
                , test "a wild with a number card is a pair with the number card's rank" <|
                    \() ->
                        Expect.equal
                            (set [ jack, blueTwo ])
                            (Just (Pair Two))
                , test "two wild cards is NOT a pair" <|
                    \() ->
                        Expect.notEqual (set [ jack, queen ]) (Just (Pair Jack))
                , test "one wild card is a single" <|
                    \() ->
                        Expect.equal (set [ jack ]) (Just (Single Jack))
                , test "two matched spot cards plus three wilds is a five-of-a-kind" <|
                    \() ->
                        Expect.equal
                            (set [ blueTwo, greenTwo, jack, queen, king ])
                            (Just (FiveOfAKind Two))
                , test "a pair of tens plus two wilds is a four-of-a-kind" <|
                    \() ->
                        Expect.equal
                            (set [ blueTen, greenTen, jack, king ])
                            (Just (FourOfAKind Ten))
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
                , test "three wild cards is not a set" <|
                    \() ->
                        Expect.equal (set [ jack, queen, king ]) Nothing
                , test "four distinct, same-suited odd cards is a bomb" <|
                    \() ->
                        Expect.equal
                            (bomb [ redThree, redFive, redSeven, redNine ])
                            (Just Suited)
                , test "A suited bomb is a suited bomb regardless of card order" <|
                    \() ->
                        Expect.equal
                            (bomb [ redFive, redNine, redThree, redSeven ])
                            (Just Suited)
                , test "four distinct odd cards, with distinct suits, is a bomb" <|
                    \() ->
                        Expect.equal
                            (bomb [ blueThree, greenFive, redSeven, yellowNine ])
                            (Just Rainbow)
                , test "four distinct odd cards, with 2-3 suits, is a not bomb" <|
                    \() ->
                        Expect.equal
                            (bomb [ blueThree, redFive, redSeven, yellowNine ])
                            Nothing
                ]
            , describe "Haggis.Combination.Sequence"
                [ test "empty set of cards is not a sequence" <|
                    \() ->
                        Expect.equal (sequence []) [ Nothing ]
                , test "three consecutive singles is a sequence" <|
                    \() ->
                        Expect.equal
                            (sequence [ blueTwo, blueThree, blueFour ])
                            [ Just (RunOfSingles 3 Four) ]
                , test "three nonconsecutive singles is not a sequence" <|
                    \() ->
                        Expect.equal
                            (sequence [ blueTwo, blueThree, redThree ])
                            [ Nothing ]
                , test "two consecutive singles is not a sequence" <|
                    \() ->
                        Expect.equal
                            (sequence [ blueTwo, blueThree ])
                            [ Nothing ]
                , test "three wildcards is not a sequence" <|
                    \() ->
                        Expect.equal
                            (sequence [ jack, queen, king ])
                            [ Nothing ]
                , test "two consecutive pairs is a sequence" <|
                    \() ->
                        Expect.equal
                            (sequence [ blueTwo, greenTwo, blueThree, greenThree ])
                            [ Just (RunOfPairs 2 Three) ]
                , test "card order should not affect sequence identification" <|
                    \() ->
                        Expect.equal
                            (sequence [ greenTwo, blueThree, greenThree, blueTwo ])
                            [ Just (RunOfPairs 2 Three) ]
                , test "one spot card and two wildcards is a run of singles" <|
                    \() ->
                        Expect.equal
                            (sequence [ blueTwo, jack, king ])
                            [ Just (RunOfSingles 3 Four) ]
                , test "a ten and three wildcards is a run of singles and a run of pairs" <|
                    \() ->
                        Expect.equal
                            (sequence [ blueTen, jack, queen, king ])
                            [ Just (RunOfSingles 4 King), Just (RunOfPairs 2 Jack) ]
                , test "one wildcard can fill a one rank gap between two singles to form a run" <|
                    \() ->
                        Expect.equal
                            (sequence [ blueTwo, blueFour, king ])
                            [ Just (RunOfSingles 3 Four) ]
                , test "one wildcard CANNOT fill a 2+ rank gap between two singles to form a run" <|
                    \() ->
                        Expect.equal
                            (sequence [ greenTwo, greenFive, king ])
                            [ Nothing ]
                , test "two wildcards can fill a 2 rank gap between two singles to form a run" <|
                    \() ->
                        Expect.equal
                            (sequence [ greenTwo, greenFive, king, jack ])
                            [ Just (RunOfSingles 4 Five) ]
                , test "one spot card and three wildcards could be a run of singles or a run of pairs" <|
                    \() ->
                        Expect.equal
                            (sequence [ blueTwo, jack, queen, king ])
                            [ Just (RunOfSingles 4 Five), Just (RunOfPairs 2 Three) ]
                , test "wild card should sub for missing card in run of pairs" <|
                    \() ->
                        Expect.equal
                            (sequence [ blueTwo, greenTwo, jack, greenThree ])
                            [ Just (RunOfPairs 2 Three) ]
                , test "a pair of tens plus two wilds is a run of pairs (T-T-J-J)" <|
                    \() ->
                        Expect.equal
                            (sequence [ blueTen, greenTen, jack, king ])
                            [ Just (RunOfPairs 2 Jack) ]
                , test "can fill holes in run of pairs that is longer than 3 ranks" <|
                    \() ->
                        Expect.equal
                            (sequence [ blueTwo, greenTwo, blueThree, greenThree, blueFour, greenFive, jack, king ])
                            [ Just (RunOfPairs 4 Five) ]
                , test "a consecutive single and a pair of spot cards, plus 3 wildcards, could be a run of pairs or a run of triples" <|
                    \() ->
                        Expect.equal
                            (sequence [ blueTwo, greenTwo, greenThree, jack, queen, king ])
                            [ Just (RunOfPairs 3 Four), Just (RunOfTriples 2 Three) ]
                , test "two consecutive pairs and two wildcards could be a run of pairs or a run of triples" <|
                    \() ->
                        Expect.equal
                            (sequence [ blueTwo, blueThree, greenTwo, greenThree, jack, queen ])
                            [ Just (RunOfPairs 3 Four), Just (RunOfTriples 2 Three) ]
                ]
            , describe "Haggis.Cards.subsets"
                [ test "the subsets of no cards is a set containing the set with no cards" <|
                    \() ->
                        Expect.equal (subsets []) [ [] ]
                , test "the subsets of one card is a set with no cards and a set with the one card" <|
                    \() ->
                        Expect.equal (subsets [ blueTwo ]) [ [ blueTwo ], [] ]
                , test "the subsets of a pair is the pair, each of its singles, and the empty set" <|
                    \() ->
                        Expect.equal
                            (subsets [ blueTwo, greenTwo ])
                            [ [ blueTwo, greenTwo ], [ blueTwo ], [ greenTwo ], [] ]
                , test "the subsets of 3 cards is the 3 cards, each card paired, each card alone, and the empty set" <|
                    \() ->
                        Expect.equal
                            (subsets [ greenTwo, greenThree, jack ])
                            [ [ greenTwo, greenThree, jack ]
                            , [ greenTwo, greenThree ]
                            , [ greenTwo, jack ]
                            , [ greenTwo ]
                            , [ greenThree, jack ]
                            , [ greenThree ]
                            , [ jack ]
                            , []
                            ]
                , test "can find all sequences contained in a set of cards" <|
                    \() ->
                        Expect.equal
                            (Hand.collectSequences [ greenTwo, greenThree, jack ])
                            [ RunOfSingles 3 Four ]
                , test "can find all sets contained in a set of cards" <|
                    \() ->
                        Expect.equal
                            (Hand.collectSets [ greenTwo, greenThree, jack ])
                            [ Pair Two
                            , Single Two
                            , Pair Three
                            , Single Three
                            , Single Jack
                            ]
                , test "can find all bombs contained in a set of cards" <|
                    \() ->
                        Expect.equal
                            (Hand.collectBombs
                                [ redThree, greenThree, redFive, redSeven, orangeSeven, redNine, yellowNine ]
                            )
                            [ Suited, Rainbow ]
                , test "can find all sets contained in a full hand of cards" <|
                    \() ->
                        Expect.equal
                            (Hand.collectSets hand)
                            [ Pair Two
                            , Single Two
                            , Single Two
                            , Triple Three
                            , Pair Three
                            , Pair Three
                            , Single Three
                            , Pair Three
                            , Single Three
                            , Single Three
                            , Single Four
                            , Pair Five
                            , Single Five
                            , Single Five
                            , Pair Seven
                            , Single Seven
                            , Single Seven
                            , Pair Nine
                            , Single Nine
                            , Single Nine
                            , Pair Ten
                            , Single Ten
                            , Single Ten
                            ]
                , test "can find all bombs contained in a full hand of cards" <|
                    \() ->
                        Expect.equal
                            (Hand.collectBombs hand)
                            [ Rainbow
                            , Rainbow
                            , Rainbow
                            , Rainbow
                            , Suited
                            , Rainbow
                            , Rainbow
                            ]
                , test "can find all sequences contained in a full hand of cards" <|
                    \() ->
                        Expect.equal
                            (Hand.collectSequences hand)
                            [ RunOfPairs 2 Three, RunOfSingles 3 Four ]
                ]
            ]
        ]


blueTwo : Card
blueTwo =
    { suit = Blue, rank = Two, order = 2 }


greenTwo : Card
greenTwo =
    { suit = Green, rank = Two, order = 2 }


blueThree : Card
blueThree =
    { suit = Blue, rank = Three, order = 3 }


greenThree : Card
greenThree =
    { suit = Green, rank = Three, order = 3 }


redThree : Card
redThree =
    { suit = Red, rank = Three, order = 3 }


blueFour : Card
blueFour =
    { suit = Blue, rank = Four, order = 4 }


redFive : Card
redFive =
    { suit = Red, rank = Five, order = 5 }


greenFive : Card
greenFive =
    { suit = Green, rank = Five, order = 5 }


redSeven : Card
redSeven =
    { suit = Red, rank = Seven, order = 7 }


orangeSeven : Card
orangeSeven =
    { suit = Orange, rank = Seven, order = 7 }


redNine : Card
redNine =
    { suit = Red, rank = Nine, order = 9 }


yellowNine : Card
yellowNine =
    { suit = Yellow, rank = Nine, order = 9 }


blueTen : Card
blueTen =
    { suit = Blue, rank = Ten, order = 10 }


greenTen : Card
greenTen =
    { suit = Green, rank = Ten, order = 10 }


jack : Card
jack =
    { suit = Wild, rank = Jack, order = 11 }


queen : Card
queen =
    { suit = Wild, rank = Queen, order = 12 }


king : Card
king =
    { suit = Wild, rank = King, order = 13 }


hand : Hand
hand =
    [ blueTwo
    , greenTwo
    , blueThree
    , redThree
    , greenThree
    , blueFour
    , redFive
    , greenFive
    , redSeven
    , orangeSeven
    , redNine
    , yellowNine
    , blueTen
    , greenTen
    ]
