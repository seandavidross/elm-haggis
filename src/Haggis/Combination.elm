module Haggis.Combination
    exposing
        ( Combination(..)
        , Set(..)
        , Sequence(..)
        , Bomb(..)
        , set
        , sequence
        , bomb
        , distribute
        , collectSets
        )

import Haggis.Card exposing (..)
import List exposing (..)
import List.Extra exposing (..)
import Debug exposing (..)


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


type alias ListOfSets =
    List (List Card)


type Sequence
    = RunOfSingles
    | RunOfPairs
    | RunOfTriples
    | RunOfFourOfAKinds
    | RunOfFiveOfAKinds
    | RunOfSixOfAKinds


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


{-| dropDuplicates is based on code from elm-community/elm-list-extras
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

        maybeSequences =
            distribute wildcards (collectSets spotcards)
    in
        if length spotcards == 0 || length cards < 3 then
            [ Nothing ]
        else
            map (tryToFormSequence) maybeSequences


tryToFormSequence : ListOfSets -> Maybe Sequence
tryToFormSequence maybeSequence =
    if canFormSequence maybeSequence then
        makeSequence maybeSequence
    else
        Nothing


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

-}
distribute : List Card -> ListOfSets -> List ListOfSets
distribute wildcards sets =
    case sortBy (.order) wildcards of
        [] ->
            [ sets ]

        [ w ] ->
            [ distributeOneWildCard w sets ]

        [ w, w' ] ->
            [ distributeOneWildCard w' <|
                distributeOneWildCard w sets
            ]

        [ w, w', w'' ] ->
            [ distributeOneWildCard w'' <|
                distributeOneWildCard w' <|
                    distributeOneWildCard w sets
            ]

        otherwise ->
            [ sets ]


distributeOneWildCard : Card -> ListOfSets -> ListOfSets
distributeOneWildCard wildcard sets =
    case sets of
        [] ->
            [ [ wildcard ] ]

        set :: [] ->
            let
                spotcard =
                    Maybe.withDefault wildcard (head set)
            in
                [ set, [ declare wildcard spotcard.suit (spotcard.order + 1) ] ]

        firstSet :: secondSet :: rest ->
            case rest of
                thirdSet :: [] ->
                    [ firstSet ] ++ (distributeAcrossTwoSets wildcard secondSet thirdSet)

                otherwise ->
                    (distributeAcrossTwoSets wildcard firstSet secondSet) ++ rest


declare : Card -> Suit -> Int -> Card
declare wildcard suit order =
    { wildcard | suit = suit, order = order }


distributeAcrossTwoSets wildcard firstSet secondSet =
    let
        firstCard =
            Maybe.withDefault wildcard (head firstSet)

        secondCard =
            Maybe.withDefault wildcard (head secondSet)

        addWildToSet order set =
            (append set [ declare wildcard (missingSuit firstSet secondSet) order ])
    in
        if allRanksConsecutive [ maybe firstCard, maybe secondCard ] then
            if length firstSet == length secondSet then
                [ firstSet, secondSet, [ declare wildcard secondCard.suit (secondCard.order + 1) ] ]
            else if length firstSet < length secondSet then
                [ (addWildToSet firstCard.order firstSet), secondSet ]
            else
                [ firstSet, (addWildToSet secondCard.order secondSet) ]
        else
            [ firstSet, [ declare wildcard firstCard.suit (firstCard.order + 1) ], secondSet ]


collectSets : List Card -> ListOfSets
collectSets cards =
    List.Extra.groupWhileTransitively (equal) (sortBy (.order) cards)


canFormSequence : ListOfSets -> Bool
canFormSequence sets =
    allSetsSameSize sets
        && allSetsSameSuits sets
        && allSetsConsecutive sets


allSetsSameSize : ListOfSets -> Bool
allSetsSameSize sets =
    case sets of
        first :: rest ->
            all (\set -> length set == length first) rest

        otherwise ->
            False


allSetsSameSuits : ListOfSets -> Bool
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


missingSuit : List Card -> List Card -> Suit
missingSuit set set' =
    case sortBy (length) [ collectSuits set, collectSuits set' ] of
        [ shortSuits, longSuits ] ->
            Maybe.withDefault Wild
                (List.Extra.find (\suit -> notMember suit shortSuits) longSuits)

        otherwise ->
            Wild


allSetsConsecutive : ListOfSets -> Bool
allSetsConsecutive sets =
    allRanksConsecutive (collectOneofEachRank sets)


collectOneofEachRank : ListOfSets -> List (Maybe Card)
collectOneofEachRank =
    map (\set -> head set)


allRanksConsecutive : List (Maybe Card) -> Bool
allRanksConsecutive cards =
    case cards of
        [] ->
            False

        card :: [] ->
            case card of
                Just card ->
                    True

                Nothing ->
                    False

        firstCard :: secondCard :: rest ->
            case ( firstCard, secondCard ) of
                ( Just firstCard, Just secondCard ) ->
                    ((firstCard.order + 1) == secondCard.order)
                        && allRanksConsecutive (maybe secondCard :: rest)

                otherwise ->
                    False


maybe : Card -> Maybe Card
maybe card =
    Maybe.map identity (Just card)


makeSequence : ListOfSets -> Maybe Sequence
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

                otherwise ->
                    Nothing

        otherwise ->
            Nothing
