module Tests exposing (..)

import Expect exposing (..)
import Haggis.Card as Card exposing (..)
import Haggis.Cards as Cards exposing (..)
import Haggis.Combination as Combo exposing (..)
import Haggis.Hand as Hand exposing (..)
import Haggis.Deck as Deck exposing (..)
import Random exposing (..)
import Test exposing (..)


all : Test
all =
    describe "Haggis"
        [ describe "Haggis.Card"
            [ test "new card has correct suit" <|
                \() ->
                    Expect.equal (Card.suit blueTwo) Blue
            , test "new card has correct rank" <|
                \() ->
                    Expect.equal (Card.rank blueTwo) Two
            , test "new card has correct points" <|
                \() ->
                    Expect.equal (Card.points redSeven) 1
            , test "card with lower rank less than card with higher rank" <|
                \() ->
                    Expect.lessThan (Card.order redSeven) (Card.order blueTwo)
            , test "two cards with the same rank are equal" <|
                \() ->
                    Expect.equal (Card.order blueTwo) (Card.order greenTwo)
            , test "non-matching single cards do not match" <|
                \() ->
                    Expect.notEqual blueTwo redSeven
            ]
        , describe "Haggis.Combination"
            [ describe "Haggis.Combination.Set"
                [ test "one card is a single" <|
                    \() ->
                        Expect.equal (Combo.set [ redSeven ]) (Just (Single Seven))
                , test "two cards of matching rank are a pair" <|
                    \() ->
                        Expect.equal (Combo.set [ blueTwo, greenTwo ]) (Just (Pair Two))
                , test "two cards with unmatched ranks are not a combination" <|
                    \() ->
                        Expect.equal (Combo.set [ blueTwo, redSeven ]) Nothing
                , test "two matched number cards with one wild is a triple" <|
                    \() ->
                        Expect.equal
                            (Combo.set [ blueTwo, greenTwo, jack ])
                            (Just (Triple Two))
                , test "a wild with a number card is a pair with the number card's rank" <|
                    \() ->
                        Expect.equal
                            (Combo.set [ jack, blueTwo ])
                            (Just (Pair Two))
                , test "two wild cards is NOT a pair" <|
                    \() ->
                        Expect.notEqual (Combo.set [ jack, queen ]) (Just (Pair Jack))
                , test "one wild card is a single" <|
                    \() ->
                        Expect.equal (Combo.set [ jack ]) (Just (Single Jack))
                , test "two matched spot cards plus three wilds is a five-of-a-kind" <|
                    \() ->
                        Expect.equal
                            (Combo.set [ blueTwo, greenTwo, jack, queen, king ])
                            (Just (FiveOfAKind Two))
                , test "a pair of tens plus two wilds is a four-of-a-kind" <|
                    \() ->
                        Expect.equal
                            (Combo.set [ blueTen, greenTen, jack, king ])
                            (Just (FourOfAKind Ten))
                ]
            , describe "Haggis.Combination.Bomb"
                [ test "one wild card is not a bomb" <|
                    \() ->
                        Expect.equal (Combo.bomb [ king ]) Nothing
                , test "two wild cards is a bomb" <|
                    \() ->
                        Expect.equal (Combo.bomb [ queen, king ]) (Just QK)
                , test "two wild cards is a bomb, order does not matter" <|
                    \() ->
                        Expect.equal (Combo.bomb [ king, jack ]) (Just JK)
                , test "three wild cards is a bomb" <|
                    \() ->
                        Expect.equal (Combo.bomb [ jack, queen, king ]) (Just JQK)
                , test "three wild cards is not a set" <|
                    \() ->
                        Expect.equal (Combo.set [ jack, queen, king ]) Nothing
                , test "four distinct, same-suited odd cards is a bomb" <|
                    \() ->
                        Expect.equal
                            (Combo.bomb [ redThree, redFive, redSeven, redNine ])
                            (Just Suited)
                , test "A suited bomb is a suited bomb regardless of card order" <|
                    \() ->
                        Expect.equal
                            (Combo.bomb [ redFive, redNine, redThree, redSeven ])
                            (Just Suited)
                , test "four distinct odd cards, with distinct suits, is a bomb" <|
                    \() ->
                        Expect.equal
                            (Combo.bomb [ blueThree, greenFive, redSeven, yellowNine ])
                            (Just Rainbow)
                , test "four distinct odd cards, with 2-3 suits, is a not bomb" <|
                    \() ->
                        Expect.equal
                            (Combo.bomb [ blueThree, redFive, redSeven, yellowNine ])
                            Nothing
                ]
            , describe "Haggis.Combination.Sequence"
                [ test "empty set of cards is not a sequence" <|
                    \() ->
                        Expect.equal (Combo.sequence []) []
                , test "three consecutive singles with mixed suits is NOT a sequence" <|
                    \() ->
                        Expect.equal
                            (Combo.sequence [ blueTwo, redThree, blueFour ])
                            []
                , test "two consecutive pairs with mixed suits is NOT a sequence" <|
                    \() ->
                        Expect.equal
                            (Combo.sequence [ blueTwo, greenTwo, redThree, blueThree ])
                            []
                , test "three consecutive singles is a sequence" <|
                    \() ->
                        Expect.equal
                            (Combo.sequence [ blueTwo, blueThree, blueFour ])
                            [ RunOfSingles 3 Four ]
                , test "three nonconsecutive singles is not a sequence" <|
                    \() ->
                        Expect.equal
                            (Combo.sequence [ blueTwo, blueThree, redThree ])
                            []
                , test "two consecutive singles is not a sequence" <|
                    \() ->
                        Expect.equal
                            (Combo.sequence [ blueTwo, blueThree ])
                            []
                , test "three wildcards is not a sequence" <|
                    \() ->
                        Expect.equal
                            (Combo.sequence [ jack, queen, king ])
                            []
                , test "two consecutive pairs is a sequence" <|
                    \() ->
                        Expect.equal
                            (Combo.sequence [ blueTwo, greenTwo, blueThree, greenThree ])
                            [ RunOfPairs 2 Three ]
                , test "card order should not affect sequence identification" <|
                    \() ->
                        Expect.equal
                            (Combo.sequence [ greenTwo, blueThree, greenThree, blueTwo ])
                            [ RunOfPairs 2 Three ]
                , test "one spot card and two wildcards is a run of singles" <|
                    \() ->
                        Expect.equal
                            (Combo.sequence [ blueTwo, jack, king ])
                            [ RunOfSingles 3 Four ]
                , test "a ten and three wildcards is a run of singles and a run of pairs" <|
                    \() ->
                        Expect.equal
                            (Combo.sequence [ blueTen, jack, queen, king ])
                            [ RunOfSingles 4 King, RunOfPairs 2 Jack ]
                , test "one wildcard can fill a one rank gap between two singles to form a run" <|
                    \() ->
                        Expect.equal
                            (Combo.sequence [ blueTwo, blueFour, king ])
                            [ RunOfSingles 3 Four ]
                , test "one wildcard CANNOT fill a 2+ rank gap between two singles to form a run" <|
                    \() ->
                        Expect.equal
                            (Combo.sequence [ greenTwo, greenFive, king ])
                            []
                , test "two wildcards can fill a 2 rank gap between two singles to form a run" <|
                    \() ->
                        Expect.equal
                            (Combo.sequence [ greenTwo, greenFive, king, jack ])
                            [ RunOfSingles 4 Five ]
                , test "one spot card and three wildcards could be a run of singles or a run of pairs" <|
                    \() ->
                        Expect.equal
                            (Combo.sequence [ blueTwo, jack, queen, king ])
                            [ RunOfSingles 4 Five, RunOfPairs 2 Three ]
                , test "wild card should sub for missing card in run of pairs" <|
                    \() ->
                        Expect.equal
                            (Combo.sequence [ blueTwo, greenTwo, jack, greenThree ])
                            [ RunOfPairs 2 Three ]
                , test "a pair of tens plus two wilds is a run of pairs (T-T-J-J)" <|
                    \() ->
                        Expect.equal
                            (Combo.sequence [ blueTen, greenTen, jack, king ])
                            [ RunOfPairs 2 Jack ]
                , test "can fill holes in run of pairs that is longer than 3 ranks" <|
                    \() ->
                        Expect.equal
                            (Combo.sequence [ blueTwo, greenTwo, blueThree, greenThree, blueFour, greenFive, jack, king ])
                            [ RunOfPairs 4 Five ]
                , test "a consecutive single and a pair of spot cards, plus 3 wildcards, could be a run of pairs or a run of triples" <|
                    \() ->
                        Expect.equal
                            (Combo.sequence [ blueTwo, greenTwo, greenThree, jack, queen, king ])
                            [ RunOfPairs 3 Four, RunOfTriples 2 Three ]
                , test "two consecutive pairs and two wildcards could be a run of pairs or a run of triples" <|
                    \() ->
                        Expect.equal
                            (Combo.sequence [ blueTwo, blueThree, greenTwo, greenThree, jack, queen ])
                            [ RunOfPairs 3 Four, RunOfTriples 2 Three ]
                ]
            , describe "Haggis.Cards.subsets"
                [ test "the subsets of no cards is a set containing the set with no cards" <|
                    \() ->
                        Expect.equal (Cards.subsets []) [ [] ]
                , test "the subsets of one card is a set with no cards and a set with the one card" <|
                    \() ->
                        Expect.equal (Cards.subsets [ blueTwo ]) [ [ blueTwo ], [] ]
                , test "the subsets of a pair is the pair, each of its singles, and the empty set" <|
                    \() ->
                        Expect.equal
                            (Cards.subsets [ blueTwo, greenTwo ])
                            [ [ blueTwo, greenTwo ], [ blueTwo ], [ greenTwo ], [] ]
                , test "the subsets of 3 cards is the 3 cards, each card paired, each card alone, and the empty set" <|
                    \() ->
                        Expect.equal
                            (Cards.subsets [ greenTwo, greenThree, jack ])
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
                                [ redThree
                                , greenThree
                                , redFive
                                , redSeven
                                , orangeSeven
                                , redNine
                                , yellowNine
                                ]
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
            , describe "Haggis.Deck"
                [ test "can shuffle an empty deck" <|
                    \() ->
                        let
                            seed =
                                Random.initialSeed 1
                        in
                            case Deck.shuffle seed [] of
                                ( deck, _ ) ->
                                    Expect.equal deck []
                , test "can shuffle a deck" <|
                    \() ->
                        let
                            seed =
                                Random.initialSeed 1

                            stock =
                                hand

                            shuffled =
                                [ greenThree
                                , blueThree
                                , redThree
                                , greenTen
                                , greenTwo
                                , redFive
                                , redNine
                                , blueFour
                                , blueTen
                                , blueTwo
                                , redSeven
                                , greenFive
                                , yellowNine
                                , orangeSeven
                                ]
                        in
                            case Deck.shuffle seed stock of
                                ( deck, _ ) ->
                                    Expect.equal deck shuffled
                , test "can deal a hand of 3 cards" <|
                    \() ->
                        let
                            seed =
                                Random.initialSeed 1

                            stock =
                                hand
                        in
                            case Deck.deal 3 stock of
                                ( _, h ) ->
                                    Expect.equal h [ blueTwo, greenTwo, blueThree ]
                , test "dealing 3 cards removes those cards from deck" <|
                    \() ->
                        let
                            seed =
                                Random.initialSeed 1

                            stock =
                                hand
                        in
                            case Deck.deal 3 stock of
                                ( deck, _ ) ->
                                    Expect.equal
                                        deck
                                        [ redThree
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
                , test "dealing a negative hand size gets an empty hand" <|
                    \() ->
                        let
                            seed =
                                Random.initialSeed 1

                            stock =
                                hand
                        in
                            case Deck.deal -3 stock of
                                ( _, h ) ->
                                    Expect.equal h []
                , test "dealing a negative hand size removes no cards from deck" <|
                    \() ->
                        let
                            seed =
                                Random.initialSeed 1

                            stock =
                                hand
                        in
                            case Deck.deal -3 stock of
                                ( deck, _ ) ->
                                    Expect.equal deck stock
                , test "dealing more cards than are in the deck makes a hand of all cards" <|
                    \() ->
                        let
                            seed =
                                Random.initialSeed 1

                            stock =
                                hand
                        in
                            case Deck.deal 15 stock of
                                ( _, h ) ->
                                    Expect.equal h stock
                , test "dealing more cards than are in the deck makes an empty deck" <|
                    \() ->
                        let
                            seed =
                                Random.initialSeed 1

                            stock =
                                hand
                        in
                            case Deck.deal 15 stock of
                                ( deck, _ ) ->
                                    Expect.equal deck []
                , test "find out which cards get dealt to hand from full deck" <|
                    \() ->
                        let
                            ( seed, deck, hand ) =
                                dealTestHand
                        in
                            Expect.equal
                                hand
                                [ Card Yellow Two 2 0
                                , Card Green Two 2 0
                                , Card Orange Three 3 1
                                , Card Red Three 3 1
                                , Card Blue Three 3 1
                                , Card Orange Four 4 0
                                , Card Yellow Four 4 0
                                , Card Red Six 6 0
                                , Card Red Seven 7 1
                                , Card Red Eight 8 0
                                , Card Orange Eight 8 0
                                , Card Green Nine 9 1
                                , Card Red Ten 10 0
                                , Card Yellow Ten 10 0
                                ]
                , test "find all sets in hand dealt from full deck" <|
                    \() ->
                        let
                            ( seed, deck, hand ) =
                                dealTestHand
                        in
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
                                , Pair Four
                                , Single Four
                                , Single Four
                                , Single Six
                                , Single Seven
                                , Pair Eight
                                , Single Eight
                                , Single Eight
                                , Single Nine
                                , Pair Ten
                                , Single Ten
                                , Single Ten
                                ]
                , test "find all sequences in hand dealt from full deck" <|
                    \() ->
                        let
                            ( seed, deck, hand ) =
                                dealTestHand
                        in
                            Expect.equal
                                (Hand.collectSequences hand)
                                [ RunOfSingles 3 Eight ]
                ]
            ]
        ]


blueTwo : Card
blueTwo =
    Card Blue Two 2 0


greenTwo : Card
greenTwo =
    Card Green Two 2 0


blueThree : Card
blueThree =
    Card Blue Three 3 1


greenThree : Card
greenThree =
    Card Green Three 3 1


redThree : Card
redThree =
    Card Red Three 3 1


blueFour : Card
blueFour =
    Card Blue Four 4 0


redFive : Card
redFive =
    Card Red Five 5 1


greenFive : Card
greenFive =
    Card Green Five 5 1


redSeven : Card
redSeven =
    Card Red Seven 7 1


orangeSeven : Card
orangeSeven =
    Card Orange Seven 7 1


redNine : Card
redNine =
    Card Red Nine 9 1


yellowNine : Card
yellowNine =
    Card Yellow Nine 9 1


blueTen : Card
blueTen =
    Card Blue Ten 10 0


greenTen : Card
greenTen =
    Card Green Ten 10 0


jack : Card
jack =
    Card Wild Jack 11 2


queen : Card
queen =
    Card Wild Queen 12 3


king : Card
king =
    Card Wild King 13 5


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


dealTestHand : ( Seed, Deck, Hand )
dealTestHand =
    let
        seed =
            Random.initialSeed 4

        ( deck, _ ) =
            Deck.shuffle seed Deck.stock

        ( _, hand ) =
            Deck.deal 14 deck

        sortedHand =
            Hand.sort hand
    in
        ( seed, deck, sortedHand )
