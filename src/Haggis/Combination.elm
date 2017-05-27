module Haggis.Combination
    exposing
        ( Combination(..)
        , Set(..)
        , Sequence(..)
        , Bomb(..)
        , set
        , sequence
        , bomb
        )

import Haggis.Card exposing (..)
import List exposing (..)
import List.Extra exposing (..)


type Combination
    = Set
    | Sequence
    | Bomb


type Set
    = Single
    | Pair
    | Triple
    | FourOfAKind
    | FiveOfAKind
    | SixOfAKind
    | SevenOfAKind
    | EightOfAKind


type Sequence
    = RunOfSingles
    | RunOfPairs
    | RunOfTriples
    | RunOfFourOfAKinds
    | RunOfFiveOfAKinds
    | RunOfSixOfAKinds
    | RunOfSevenOfAKinds
    | RunOfEightOfAKinds


type Bomb
    = Rainbow
    | JQ
    | JK
    | QK
    | JQK
    | Suited



-- SET


set : List Card -> Maybe Set
set cards =
    let
        ( spotcards, wildcards ) =
            partition (isSpotCard) cards
    in
        if allSameRank spotcards then
            makeSet cards
        else if length wildcards == 1 && length cards == 1 then
            Just Single
        else
            Nothing


allSameRank : List Card -> Bool
allSameRank cards =
    case cards of
        [] ->
            False

        first :: rest ->
            all (equal first) rest


makeSet : List Card -> Maybe Set
makeSet cards =
    case length cards of
        1 ->
            Just Single

        2 ->
            Just Pair

        3 ->
            Just Triple

        4 ->
            Just FourOfAKind

        5 ->
            Just FiveOfAKind

        6 ->
            Just SixOfAKind

        7 ->
            Just SevenOfAKind

        8 ->
            Just EightOfAKind

        otherwise ->
            Nothing



-- BOMB


bomb : List Card -> Maybe Bomb
bomb cards =
    let
        ranks =
            cards |> sortBy (.order) |> map .rank
    in
        case ranks of
            [ Jack, Queen ] ->
                Just JQ

            [ Jack, King ] ->
                Just JK

            [ Queen, King ] ->
                Just QK

            [ Jack, Queen, King ] ->
                Just JQK

            [ Three, Five, Seven, Nine ] ->
                if allSameSuit cards then
                    Just Suited
                else if hasFourSuits cards then
                    Just Rainbow
                else
                    Nothing

            otherwise ->
                Nothing


allSameSuit : List Card -> Bool
allSameSuit cards =
    case cards of
        first :: rest ->
            all (hasSameSuit first) rest

        otherwise ->
            False


hasSameSuit : Card -> Card -> Bool
hasSameSuit card card' =
    card.suit == card'.suit


hasFourSuits : List Card -> Bool
hasFourSuits cards =
    let
        numberOfSuits =
            cards
                |> map .suit
                |> dropDuplicates
                |> length
    in
        numberOfSuits == 4



{-
   dropDuplicates is based on code from elm-community/elm-list-extras
-}


dropDuplicates : List Suit -> List Suit
dropDuplicates suits =
    dropDuplicates' [] suits


dropDuplicates' : List Suit -> List Suit -> List Suit
dropDuplicates' existing remaining =
    case remaining of
        [] ->
            []

        first :: rest ->
            if List.member first existing then
                dropDuplicates' existing rest
            else
                first :: dropDuplicates' (first :: existing) rest



-- SEQUENCE
-- TODO still need to handle Wild cards...


sequence : List Card -> List (Maybe Sequence)
sequence cards =
    let
        ( spotcards, wildcards ) =
            partition (isSpotCard) cards

        sets =
            collectSets cards

        --distribute wildcards (collectSets spotcards)
    in
        if length spotcards == 0 || length cards < 3 then
            [ Nothing ]
        else if canFormSequence sets then
            [ makeSequence sets ]
            -- List.map (makeSequence) sets
        else
            [ Nothing ]


{-| To determine the possible sequences that can be formed using wildcards,
we first need to distribute the wildcards evenly amongst the sets of spot cards.
As we do so, we need to change the wildcard's suit to fit the grouping of suits
in its set, and we need change the ordinal value of the wildcard's rank so that
it matches the rank of the other cards in the set.

EXAMPLES:

Let c_N be a card with rank N and let j_N, q_N, k_N be a wild card's rank after
being distibuted, then:

    distribute  [ j_11, q_12 ]   [ [ c_2 ] ]

returns a RunOfSingles

    [ [ c_2 ], [ j_3 ], [ q_4 ] ]

But wildcards can be used to form more than one type of Sequence, so:

    distribute  [ j_11, q_12, k_13 ]   [ [ c_2 ], [ c_3, c_3' ] ]

returns a RunOfPairs

    [ [ c_2, j_2 ], [ c_3, c_3' ], [ q_4, k_4 ] ]

*AND* a RunOfTriples

    [ [ c_2, j_2, q_2 ], [ c_3, c_3', k_3 ] ]

NOTE
We need at least one spot card to form a Sequence:

    distribute  [ j_11, q_12, k_13 ]    [ [] ]

returns

    [ [ j_11 ], [ q_12 ], [ k_13 ] ]

but this is NOT a Sequence, it can only ever be a Bomb.

TODO
The code below is only a placeholder that allows everything to compile.
There are currently 6 failing tests that the real code needs to make pass...

-}
distribute : List Card -> List (List Card) -> List (List (List Card))
distribute wildcards sets =
    [ append sets [ wildcards ] ]


collectSets : List Card -> List (List Card)
collectSets cards =
    List.Extra.groupWhile (equal) (sortBy (.order) cards)


canFormSequence : List (List Card) -> Bool
canFormSequence sets =
    allSetsSameSize sets
        && allSetsSameSuits sets
        && allSetsConsecutive sets


allSetsSameSize : List (List Card) -> Bool
allSetsSameSize sets =
    case sets of
        first :: rest ->
            all (\s -> length s == length first) rest

        otherwise ->
            False


allSetsSameSuits : List (List Card) -> Bool
allSetsSameSuits sets =
    case map (collectSuits) sets of
        firstGroup :: suitGroups ->
            all ((==) firstGroup) suitGroups

        otherwise ->
            False


collectSuits : List Card -> List Suit
collectSuits set =
    set
        |> map .suit
        |> sortWith (compareSuits)


allSetsConsecutive : List (List Card) -> Bool
allSetsConsecutive sets =
    allRanksConsecutive (collectOneofEachRank sets)


collectOneofEachRank : List (List Card) -> List (Maybe Card)
collectOneofEachRank =
    map (\s -> head s)


allRanksConsecutive : List (Maybe Card) -> Bool
allRanksConsecutive cards =
    case cards of
        [] ->
            False

        c :: [] ->
            case c of
                Just c ->
                    True

                Nothing ->
                    False

        c :: c' :: rest ->
            case ( c, c' ) of
                ( Just c, Just c' ) ->
                    ((c.order + 1) == c'.order)
                        && allRanksConsecutive (maybe c' :: rest)

                otherwise ->
                    False


maybe : Card -> Maybe Card
maybe card =
    Maybe.map identity (Just card)


makeSequence : List (List Card) -> Maybe Sequence
makeSequence sets =
    case sets of
        set :: _ ->
            case length set of
                1 ->
                    Just RunOfSingles

                2 ->
                    Just RunOfPairs

                3 ->
                    Just RunOfTriples

                4 ->
                    Just RunOfFourOfAKinds

                5 ->
                    Just RunOfFiveOfAKinds

                6 ->
                    Just RunOfSixOfAKinds

                7 ->
                    Just RunOfSevenOfAKinds

                8 ->
                    Just RunOfEightOfAKinds

                otherwise ->
                    Nothing

        otherwise ->
            Nothing
